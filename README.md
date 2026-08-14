# =============================================================================
# R/data_roles_licensing.R
# Sections G, I, J, K of the brief: security structure (Role -> Duty ->
# Privilege -> Entry Point) and the licensing requirement / by-role views.
# =============================================================================

#' Load USERSECGOVRELATEDOBJECTS (the Role/Duty/Privilege/Entry Point/License
#' backbone). role_filter is an optional SQL LIKE pattern (NULL = all roles).
load_security_governance_objects <- function(cnn, sql_dir, role_filter = NULL) {
  run_readonly_query(
    cnn, file.path(sql_dir, "05_user_security_governance_objects.sql"),
    params = list(RoleNameFilter = role_filter)
  )
}

#' Load LICENSINGUSERLICENSESBYROLE
load_licensing_user_license_by_role <- function(cnn, sql_dir) {
  run_readonly_query(cnn, file.path(sql_dir, "04_licensing_user_license_by_role.sql"))
}

#' Load the three *_DETAILEDVIEW licensing tables
load_licensing_requirements <- function(cnn, sql_dir, entitled_only = TRUE) {
  list(
    role      = run_readonly_query(cnn, file.path(sql_dir, "06_licensing_role_requirements.sql"),
                                    params = list(EntitledOnly = entitled_only)),
    duty      = run_readonly_query(cnn, file.path(sql_dir, "07_licensing_duty_requirements.sql"),
                                    params = list(EntitledOnly = entitled_only)),
    privilege = run_readonly_query(cnn, file.path(sql_dir, "08_licensing_privilege_requirements.sql"),
                                    params = list(EntitledOnly = entitled_only))
  )
}

#' Normalize column names defensively: detailed views can vary slightly by
#' D365FO version. This helper only renames if the expected column is absent
#' and an unambiguous case-insensitive match exists; otherwise it leaves the
#' column missing and downstream code must handle NA / "UNKNOWN".
.normalize_licensing_columns <- function(df, expected = c("ENTITLED", "ACCESSLEVEL", "SKUNAME", "SECURITYROLE")) {
  for (col in expected) {
    if (!col %in% names(df)) {
      match_idx <- which(toupper(names(df)) == col)
      if (length(match_idx) == 1) names(df)[match_idx] <- col
    }
  }
  missing <- setdiff(expected, names(df))
  if (length(missing) > 0) {
    log_warn("data_roles_licensing", "expected column(s) missing, will be treated as UNKNOWN: %s",
              paste(missing, collapse = ", "))
    for (m in missing) df[[m]] <- NA
  }
  df
}

#' analyze_security_structure(): build the Role -> Duty -> Privilege ->
#' Entry Point -> Access Level -> License Requirement backbone used by all
#' downstream role/user analyses.
#'
#' Join keys are normalized (trimmed + uppercased) before matching, because
#' fixed-length CHAR columns in SQL Server frequently carry trailing padding
#' spaces on one side of the join but not the other (secgov vs. the
#' *_DETAILEDVIEW views), which silently produces zero matches with a naive
#' merge(). The ORIGINAL (non-normalized) ROLENAME from secgov is always kept
#' for display/reporting.
#'
#' IMPORTANT: this join is composite - Role AND Entry Point - not Role alone.
#' Both secgov and the *_DETAILEDVIEW tables are already at entry-point
#' granularity (many rows per role). Joining on Role alone produces a
#' many-to-many cartesian product PER ROLE (rows_secgov_for_role x
#' rows_req_for_role), which can reach billions of rows across all roles and
#' crash merge() with an integer-overflow error ("Vektoren negativer Länge
#' sind nicht erlaubt" / "negative-length vectors are not allowed"). This
#' function therefore ALWAYS estimates the resulting row count before
#' merging and refuses to proceed if it would be unreasonably large, instead
#' of letting R crash uninformatively.
#'
#' Entry-point columns are auto-detected (secgov: RESOURCE, falling back to
#' ENTRYPOINTNAME; requirements view: AOTNAME) but can be overridden - same
#' pattern as the role columns.
#'
#' IMPORTANT (environment-specific): in some D365FO versions/extracts,
#' *_DETAILEDVIEW.SECURITYROLE is a numeric RecId (foreign key), NOT the
#' role's display name - a text-to-text join then legitimately matches
#' nothing. This function detects that case and refuses to silently produce
#' all-NA license columns; instead it logs a clear diagnostic and returns the
#' unjoined structure with license columns = "UNKNOWN", so the mismatch is
#' visible in the run log rather than hidden inside NA values (brief section
#' 3: never fabricate a result when it cannot be determined).
#'
#' @param secgov data.frame from load_security_governance_objects()
#' @param licensing_req list(role=, duty=, privilege=) from load_licensing_requirements()
#' @param role_join_column_secgov optional explicit role-name column in secgov
#' @param role_join_column_req optional explicit role-name column in licensing_req$role
#' @param entry_point_join_column_secgov optional explicit entry-point column in secgov
#' @param entry_point_join_column_req optional explicit entry-point column in licensing_req$role
#' @param max_merge_rows safety cap on the estimated merge result size (default 20 million)
#' @return data.frame, one row per (role, duty, privilege, entry point) tuple
#'         with license columns attached where resolvable, else UNKNOWN.
analyze_security_structure <- function(secgov, licensing_req,
                                         role_join_column_secgov = NULL,
                                         role_join_column_req = NULL,
                                         entry_point_join_column_secgov = NULL,
                                         entry_point_join_column_req = NULL,
                                         max_merge_rows = 20e6) {
  role_req <- .normalize_licensing_columns(licensing_req$role)

  # --- Determine join columns --------------------------------------------
  role_col_secgov <- role_join_column_secgov %||%
    names(secgov)[toupper(names(secgov)) == "ROLENAME"][1]
  role_col_req <- role_join_column_req %||%
    names(role_req)[toupper(names(role_req)) == "SECURITYROLE"][1]

  ep_col_secgov <- entry_point_join_column_secgov %||%
    c(names(secgov)[toupper(names(secgov)) == "RESOURCE"],
      names(secgov)[toupper(names(secgov)) == "ENTRYPOINTNAME"])[1]
  ep_col_req <- entry_point_join_column_req %||%
    names(role_req)[toupper(names(role_req)) == "AOTNAME"][1]

  if (is.na(role_col_secgov) || is.na(role_col_req) ||
      is.null(role_col_secgov) || is.null(role_col_req)) {
    log_warn("data_roles_licensing",
              "could not unambiguously identify role-name join columns; returning secgov unjoined (license columns = UNKNOWN)")
    secgov$ENTITLED   <- "UNKNOWN"
    secgov$ACCESSLEVEL <- "UNKNOWN"
    secgov$SKUNAME     <- "UNKNOWN"
    return(secgov)
  }

  use_composite_key <- !is.na(ep_col_secgov) && !is.na(ep_col_req) &&
    !is.null(ep_col_secgov) && !is.null(ep_col_req)

  if (!use_composite_key) {
    log_warn("data_roles_licensing", paste0(
      "no entry-point join column identified on one or both sides (secgov: %s, requirements: %s); ",
      "falling back to a Role-only join key. THIS CAN CAUSE A MASSIVE CARTESIAN PRODUCT if both ",
      "sources carry multiple rows per role (they typically do) - the row-count guard below will ",
      "abort rather than crash if that happens. Prefer setting entry_point_join_column_secgov/req ",
      "explicitly once you've identified the correct columns (candidates: secgov$RESOURCE, ",
      "requirements$AOTNAME)."),
      ep_col_secgov %||% "NOT FOUND", ep_col_req %||% "NOT FOUND")
  }

  # --- Type-mismatch guard: a numeric RecId cannot be text-joined against a
  # display name. This function refuses to guess at licensing-relevant joins.
  req_col_is_numeric <- is.numeric(role_req[[role_col_req]]) || inherits(role_req[[role_col_req]], "integer64")
  secgov_col_is_numeric <- is.numeric(secgov[[role_col_secgov]]) || inherits(secgov[[role_col_secgov]], "integer64")

  if (req_col_is_numeric != secgov_col_is_numeric) {
    log_error("data_roles_licensing",
               "JOIN TYPE MISMATCH: secgov$%s is %s but %s$%s is %s. A text-to-ID join can never match.",
               role_col_secgov, class(secgov[[role_col_secgov]])[1],
               "licensing_req$role", role_col_req, class(role_req[[role_col_req]])[1])

    candidate_text_cols <- names(role_req)[vapply(role_req, is.character, logical(1))]
    candidate_text_cols <- candidate_text_cols[grepl("ROLE|NAME", toupper(candidate_text_cols))]
    candidate_id_cols <- names(secgov)[vapply(secgov, function(x) is.numeric(x) || inherits(x, "integer64"), logical(1))]
    candidate_id_cols <- candidate_id_cols[grepl("ROLE|ID|RECID", toupper(candidate_id_cols))]

    log_error("data_roles_licensing",
               "Candidate text columns in requirements view that might hold the role NAME: %s",
               if (length(candidate_text_cols) > 0) paste(candidate_text_cols, collapse = ", ") else "(none found)")
    log_error("data_roles_licensing",
               "Candidate numeric ID columns in secgov that might hold a role RecId: %s",
               if (length(candidate_id_cols) > 0) paste(candidate_id_cols, collapse = ", ") else "(none found)")
    log_error("data_roles_licensing", paste0(
      "Set the correct pair explicitly, e.g. analyze_security_structure(secgov, licensing_req, ",
      "role_join_column_secgov = 'ROLERECID', role_join_column_req = 'SECURITYROLE') or in config.yml ",
      "under security_structure.role_join_column_secgov / role_join_column_req."))

    secgov$ENTITLED   <- "UNKNOWN"
    secgov$ACCESSLEVEL <- "UNKNOWN"
    secgov$SKUNAME     <- "UNKNOWN"
    return(secgov)
  }

  # Normalized join keys (trim + uppercase for text; identity for numeric).
  .norm_key <- function(x) {
    if (is.numeric(x) || inherits(x, "integer64")) as.character(x) else trimws(toupper(as.character(x)))
  }

  if (use_composite_key) {
    secgov$.JOINKEY   <- paste(.norm_key(secgov[[role_col_secgov]]), .norm_key(secgov[[ep_col_secgov]]), sep = "\u0001")
    role_req$.JOINKEY <- paste(.norm_key(role_req[[role_col_req]]), .norm_key(role_req[[ep_col_req]]), sep = "\u0001")
  } else {
    secgov$.JOINKEY   <- .norm_key(secgov[[role_col_secgov]])
    role_req$.JOINKEY <- .norm_key(role_req[[role_col_req]])
  }

  n_secgov_keys <- length(unique(secgov$.JOINKEY))
  n_overlap <- length(intersect(unique(secgov$.JOINKEY), unique(role_req$.JOINKEY)))

  if (n_overlap == 0 && n_secgov_keys > 0) {
    log_warn("data_roles_licensing",
              "0 of %d distinct join keys in secgov matched any key in the licensing requirements view.",
              n_secgov_keys)
    log_warn("data_roles_licensing",
              "even after normalization. Sample secgov key: '%s' | sample requirements key: '%s'.",
              utils::head(secgov[[role_col_secgov]], 1), utils::head(role_req[[role_col_req]], 1))
    log_warn("data_roles_licensing", paste0(
              "Check whether load_licensing_requirements() was called with entitled_only=TRUE and simply ",
              "returned 0 relevant rows, or whether the join column choice itself is wrong for this environment."))
  } else if (n_overlap < n_secgov_keys) {
    log_warn("data_roles_licensing",
              "only %d of %d distinct secgov join keys matched a licensing requirement record.",
              n_overlap, n_secgov_keys)
    log_warn("data_roles_licensing", "the remainder will show has_license_requirement_record = FALSE (see DQ-003).")
  }

  # --- Cartesian-product guard --------------------------------------------
  # Estimate the merge result size BEFORE calling merge(), so a mis-detected
  # join column produces a clear, actionable error instead of a cryptic
  # "negative-length vectors" crash (integer overflow inside merge.data.frame).
  secgov_counts <- table(secgov$.JOINKEY)
  req_counts <- table(role_req$.JOINKEY)
  common_keys <- intersect(names(secgov_counts), names(req_counts))
  estimated_rows <- if (length(common_keys) > 0) {
    sum(as.numeric(secgov_counts[common_keys]) * as.numeric(req_counts[common_keys]))
  } else {
    0
  }
  # Non-matching secgov rows survive via all.x = TRUE as single rows.
  estimated_rows <- estimated_rows + (nrow(secgov) - sum(secgov_counts[common_keys]))

  if (estimated_rows > max_merge_rows) {
    log_error("data_roles_licensing",
               "REFUSING TO MERGE: estimated result size is ~%.0f million rows (cap: %.0f million).",
               estimated_rows / 1e6, max_merge_rows / 1e6)
    log_error("data_roles_licensing", paste0(
               "This means the join key is too coarse (matches many-to-many instead of ~1-to-1/few) and ",
               "would otherwise crash merge() with an integer overflow ('negative-length vectors')."))
    log_error("data_roles_licensing",
               "use_composite_key was %s (secgov entry-point column: %s, requirements entry-point column: %s).",
               use_composite_key, ep_col_secgov %||% "NOT FOUND", ep_col_req %||% "NOT FOUND")
    log_error("data_roles_licensing", paste0(
               "Run dput(names(secgov)) and dput(names(licensing_req$role)) to identify the correct ",
               "entry-point-level columns and pass them via entry_point_join_column_secgov / _req."))
    secgov$ENTITLED   <- "UNKNOWN"
    secgov$ACCESSLEVEL <- "UNKNOWN"
    secgov$SKUNAME     <- "UNKNOWN"
    return(secgov)
  }

  merged <- merge(
    secgov, role_req,
    by = ".JOINKEY",
    all.x = TRUE, suffixes = c("", "_LICREQ")
  )
  merged$.JOINKEY <- NULL
  for (dup_col in c(role_col_req, ep_col_req)) {
    if (is.na(dup_col)) next
    if (paste0(dup_col, "_LICREQ") %in% names(merged)) {
      merged[[paste0(dup_col, "_LICREQ")]] <- NULL
    } else if (dup_col %in% names(merged) && !(dup_col %in% c(role_col_secgov, ep_col_secgov))) {
      merged[[dup_col]] <- NULL
    }
  }

  # Rows with no matching licensing requirement genuinely mean "not
  # licensing-relevant per this view", NOT "unknown" - but we still flag rows
  # where ENTITLED is NA (no requirement record at all) distinctly for the
  # data-quality checks (DQ-003 / DQ-004).
  merged$has_license_requirement_record <- !is.na(merged$ENTITLED)
  merged
}
