#' Benutzer-Rollenzuweisungen laden
#'
#' Lädt die Rollenzuweisungen aus:
#'
#' * SystemSecurityUserRoleEntity
#' * SystemSecurityUserRoleOrganizationEntity
#'
#' Organisationsspezifische Zuordnungen
#' haben Vorrang vor globalen Zuordnungen.
#'
#' Fehlt eine Organisationszuordnung,
#' wird ORGANIZATIONID = 'All' gesetzt.
#'
#' @param con Datenbankverbindung.
#' @param user_ids Benutzerkennungen.
#' @param roles Rollennamen oder Rollen-AOT-Identifier (SECURITYROLENAME oder
#'   SECURITYROLEIDENTIFIER). Beide Spalten werden geprüft, sodass Name und
#'   Identifier gemischt übergeben werden können.
#' @param organization_ids Organisationen.
#'
#' @return Tibble.
#'
#' @export
get_sql_user_role_assignments <- function(
  con,
  user_ids = character(),
  roles = character(),
  organization_ids = character()
){
  query <- "
SELECT DISTINCT
    T1.USERID,
    T1.SECURITYROLEIDENTIFIER,
    T1.SECURITYROLENAME,
    COALESCE(
        T2.ORGANIZATIONID,
        'All'
    ) AS ORGANIZATIONID
    -- , T2.ORGANIZATIONTYPE
FROM
    SystemSecurityUserRoleEntity T1
LEFT JOIN
    SystemSecurityUserRoleOrganizationEntity T2
    ON T1.USERID = T2.USERID
    AND T1.SECURITYROLEIDENTIFIER =
        T2.SECURITYROLEIDENTIFIER
WHERE
    1 = 1
"

  add_in_filter <- function(query, column, values) {
    if (length(values) == 0) return(query)
    value_list <- paste0("'", gsub("'", "''", values), "'", collapse = ", ")
    paste0(query, "\nAND ", column, " IN (", value_list, ")")
  }

  query <- add_in_filter(query, "T1.USERID", user_ids)

  if (length(roles) > 0L) {
    value_list <- paste0("'", gsub("'", "''", roles), "'", collapse = ", ")
    query <- paste0(
      query,
      "\nAND (T1.SECURITYROLENAME IN (", value_list,
      ") OR T1.SECURITYROLEIDENTIFIER IN (", value_list, "))"
    )
  }

  query <- add_in_filter(
    query,
    "COALESCE(T2.ORGANIZATIONID,'All')",
    organization_ids
  )

  DBI::dbGetQuery(con, query) |> tibble::as_tibble()
}
