# =============================================================================
# Application Insights Telemetrie
#
# Verbindungsweg: httr2 POST gegen die Application Insights REST-API.
# Credentials kommen ausschliesslich aus Umgebungsvariablen (.Renviron):
#   D365_APPINSIGHTS_APP_ID   – Application ID aus dem Azure Portal
#   D365_APPINSIGHTS_API_KEY  – API-Schluessel mit Berechtigung "Telemetrie lesen"
#
# Endpoint: https://api.applicationinsights.io/v1/apps/{app_id}/query
# Methode:  POST mit JSON-Body { "query": "<KQL>" }
#           (robuster als GET bei langen KQL-Abfragen wegen URL-Laengenlimit)
#
# Alle Funktionen geben bei Verbindungsfehlern einen leeren data.frame mit
# attr(status) = "UNKNOWN" zurueck -- kein Absturz, kein stop().
# =============================================================================


# Laedt eine .kql-Datei aus inst/kql/ und ersetzt {{Platzhalter}} durch Werte.
.render_kql <- function(kql_filename, vars = list()) {
  path <- system.file("kql", kql_filename, package = "D365Licensing")
  if (!nzchar(path)) stop("KQL-Datei nicht im Paket gefunden: kql/", kql_filename)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  for (nm in names(vars)) {
    text <- gsub(paste0("\\{\\{", nm, "\\}\\}"), vars[[nm]], text, fixed = FALSE)
  }
  gsub("\\{\\{[a-zA-Z_]+\\}\\}", "", text)
}


# Liest App-ID und API-Key aus den konfigurierten Umgebungsvariablen.
# Stoppt mit einer klaren Fehlermeldung wenn eine Variable fehlt oder leer ist.
.get_appinsights_credentials <- function(cfg) {
  app_id  <- Sys.getenv(cfg$telemetry$app_insights_env_var, unset = "")
  api_key <- Sys.getenv(cfg$telemetry$api_key_env_var,      unset = "")

  if (!nzchar(app_id)) {
    stop(
      "Umgebungsvariable '", cfg$telemetry$app_insights_env_var, "' ist nicht gesetzt.\n",
      "In .Renviron eintragen: ", cfg$telemetry$app_insights_env_var, "=<App-ID>\n",
      "Dann Session neu starten (Session -> Restart R). Siehe HOWTO Abschnitt 3.3."
    )
  }
  if (!nzchar(api_key)) {
    stop(
      "Umgebungsvariable '", cfg$telemetry$api_key_env_var, "' ist nicht gesetzt.\n",
      "In .Renviron eintragen: ", cfg$telemetry$api_key_env_var, "=<API-Key>\n",
      "Dann Session neu starten (Session -> Restart R). Siehe HOWTO Abschnitt 3.3."
    )
  }
  list(app_id = app_id, api_key = api_key)
}


# Wandelt die JSON-Antwort der Application Insights REST-API in einen data.frame um.
# Die API liefert: { "tables": [{ "columns": [{"name":...}], "rows": [[w1,w2,...]] }] }
#
# Jede Spalte wird separat als character-Vektor extrahiert, um List-Columns zu vermeiden.
# List-Columns entstehen bei do.call(rbind, lapply(rows, unlist)) und brechen as.POSIXct().
.parse_appinsights_response <- function(resp) {
  body <- httr2::resp_body_json(resp)
  tbl  <- body$tables[[1]]
  cols <- vapply(tbl$columns, function(col) col$name, character(1))
  rows <- tbl$rows

  if (length(rows) == 0) {
    res <- as.data.frame(matrix(nrow = 0, ncol = length(cols)), stringsAsFactors = FALSE)
    names(res) <- cols
    attr(res, "status") <- "OK_EMPTY"
    return(res)
  }

  cols_data <- lapply(seq_along(cols), function(i) {
    vapply(rows, function(row) {
      v <- row[[i]]
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
    }, character(1))
  })

  df <- as.data.frame(setNames(cols_data, cols), stringsAsFactors = FALSE)
  attr(df, "status") <- "OK"
  df
}


#' AppInsights-Verbindung testen.
#'
#' Fuehrt eine minimale KQL-Abfrage (\code{pageViews | take 1}) aus und gibt
#' den HTTP-Statuscode zurueck. Nutze diese Funktion zur Diagnose, bevor die
#' vollstaendige Analyse gestartet wird.
#'
#' Voraussetzungen (HOWTO Abschnitt 3.3):
#' \itemize{
#'   \item \code{D365_APPINSIGHTS_APP_ID} in \code{.Renviron} gesetzt
#'   \item \code{D365_APPINSIGHTS_API_KEY} in \code{.Renviron} gesetzt
#'   \item R-Session nach dem Setzen neu gestartet
#' }
#'
#' @param cfg Geladene Konfiguration aus \code{\link{load_config}}.
#' @return Unsichtbar: HTTP-Statuscode (200 = OK). Gibt eine Konsolenmeldung aus.
#' @export
test_appinsights_connection <- function(cfg) {
  creds <- .get_appinsights_credentials(cfg)

  url <- sprintf("https://api.applicationinsights.io/v1/apps/%s/query", creds$app_id)

  resp <- httr2::request(url) |>
    httr2::req_headers("x-api-key" = creds$api_key) |>
    httr2::req_url_query(query = "pageViews | take 1") |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  message("Application Insights Verbindung OK -- HTTP Status: ", status)
  invisible(status)
}


#' KQL-Abfrage gegen Application Insights ausfuehren.
#'
#' Sendet eine KQL-Abfrage per HTTP POST an die Application Insights REST-API.
#' Credentials werden aus den Umgebungsvariablen gelesen, die in \code{cfg}
#' konfiguriert sind (Standard: \code{D365_APPINSIGHTS_APP_ID} und
#' \code{D365_APPINSIGHTS_API_KEY}).
#'
#' @param environment Logischer Umgebungsname (optional, wird fuer Logging verwendet).
#' @param kql_query character, die KQL-Abfrage als String.
#' @param cfg Geladene Konfiguration aus \code{\link{load_config}}. Wird automatisch
#'   geladen wenn \code{NULL}.
#' @return \code{data.frame} mit dem Abfrageergebnis und \code{attr(status)} =
#'   \code{"OK"} oder \code{"OK_EMPTY"}. Im Fehlerfall wird \code{stop()} aufgerufen.
#' @export
run_appinsights_query <- function(environment = NULL, kql_query, cfg = NULL) {
  if (is.null(cfg)) cfg <- load_config()

  creds <- .get_appinsights_credentials(cfg)

  url <- sprintf("https://api.applicationinsights.io/v1/apps/%s/query", creds$app_id)

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers("x-api-key" = creds$api_key) |>
      httr2::req_body_json(list(query = kql_query)) |>
      httr2::req_perform(),
    error = function(e) stop("HTTP-Anfrage fehlgeschlagen: ", conditionMessage(e))
  )

  http_status <- httr2::resp_status(resp)
  if (http_status != 200L) {
    stop(sprintf(
      "Application Insights antwortete mit HTTP %s. Credentials pruefen (HOWTO 3.3).",
      http_status
    ))
  }

  .parse_appinsights_response(resp)
}


#' KQL-Abfrage mit Fehlerbehandlung ausfuehren (interne Pipeline-Funktion).
#'
#' Wrapper um \code{\link{run_appinsights_query}} fuer den internen Gebrauch in der
#' Analyse-Pipeline. Gibt bei Verbindungsfehlern einen leeren \code{data.frame} mit
#' \code{attr(status) = "UNKNOWN"} zurueck statt einen Fehler zu werfen -- die
#' Pipeline laeuft weiter, betroffene Findings erhalten Confidence = UNKNOWN.
#'
#' @param kql_query character, die KQL-Abfrage.
#' @param cfg Geladene Konfiguration (siehe \code{\link{load_config}}).
#' @param environment Logischer Umgebungsname, wird an \code{run_appinsights_query}
#'   weitergegeben.
#' @return \code{data.frame} mit \code{attr(status)} = \code{"OK"},
#'   \code{"OK_EMPTY"} oder \code{"UNKNOWN"}.
#' @export
run_kql_query <- function(kql_query, cfg, environment = NULL) {

  # Telemetrie-Aktivierung pruefen: wenn false, sofort UNKNOWN -- keine Netzwerkanfrage.
  if (!isTRUE(cfg$telemetry$enabled)) {
    log_warn("data_telemetry",
             "Telemetrie deaktiviert (telemetry.enabled: false) -- UNKNOWN wird zurueckgegeben. ",
             "Zum Aktivieren: telemetry.enabled: true in config.local.yml setzen (HOWTO 3.2).")
    res <- data.frame()
    attr(res, "status") <- "UNKNOWN"
    return(res)
  }

  # AppInsights-Abfrage ausfuehren; Verbindungsfehler werden abgefangen.
  res <- tryCatch(
    run_appinsights_query(environment = environment, kql_query = kql_query, cfg = cfg),
    error = function(e) {
      log_error("data_telemetry",
                "AppInsights-Abfrage fehlgeschlagen: %s", conditionMessage(e))
      NULL
    }
  )

  if (is.null(res)) {
    res <- data.frame()
    attr(res, "status") <- "UNKNOWN"
    return(res)
  }

  res
}


#' Telemetrie-Beruehrungspunkte pro Benutzer und EntryPoint laden.
#'
#' Laedt die KQL-Abfrage aus \code{kql/04_user_entrypoint_touches.kql} und
#' fuehrt sie gegen Application Insights aus. Das Analysefenster wird aus
#' \code{cfg$telemetry$telemetry_days} gelesen.
#'
#' Gibt einen leeren \code{data.frame} mit \code{attr(status) = "UNKNOWN"} zurueck
#' wenn die Verbindung nicht hergestellt werden kann -- kein Absturz.
#'
#' @param cfg Geladene Konfiguration (siehe \code{\link{load_config}}).
#' @param environment Logischer D365FO-Umgebungsname.
#' @param entry_point_filter Optionaler EntryPoint-Name zur Einschraenkung der Abfrage.
#' @return \code{data.frame} mit Spalten \code{user_Id}, \code{name},
#'   \code{last_seen}, \code{duration_seconds}, \code{touches}.
#' @export
load_telemetry_touches <- function(cfg, environment = NULL, entry_point_filter = NULL) {

  days <- cfg$telemetry$telemetry_days
  if (is.null(days) || !is.numeric(days) || days <= 0) {
    stop("telemetry_days muss eine positive Zahl sein (Wert: ", days, ")")
  }
  if (days < (cfg$telemetry$min_recommended_days %||% 14)) {
    log_warn("data_telemetry",
             "telemetry_days=%s liegt unter dem empfohlenen Minimum (%s).",
             days, cfg$telemetry$min_recommended_days)
  }

  filter_clause <- if (is.null(entry_point_filter)) {
    ""
  } else {
    sprintf('and name == "%s"', entry_point_filter)
  }

  kql <- .render_kql(
    "04_user_entrypoint_touches.kql",
    vars = list(telemetry_days = days, entry_point_filter = filter_clause)
  )

  res    <- run_kql_query(kql, cfg, environment = environment)
  status <- attr(res, "status")

  # Spaltentypen normalisieren -- Parser liefert alles als character.
  if (!is.null(status) && status %in% c("OK", "OK_EMPTY") && nrow(res) > 0) {

    if ("duration_seconds" %in% names(res))
      res$duration_seconds <- suppressWarnings(as.numeric(res$duration_seconds))

    if ("touches" %in% names(res))
      res$touches <- suppressWarnings(as.numeric(res$touches))

    # AppInsights liefert Timestamps in mehreren ISO-8601-Varianten:
    #   "2026-01-15T14:30:00Z"           (Standard)
    #   "2026-01-15T14:30:00.0000000Z"   (mit Mikrosekunden)
    #   "2026-01-15 14:30:00"            (ohne T und Z)
    # %Y-%m-%dT%H:%M:%OSZ deckt alle Faelle ab; Fallback ohne Format fuer den Rest.
    if ("last_seen" %in% names(res)) {
      parsed <- tryCatch({
        p <- as.POSIXct(res$last_seen, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
        if (all(is.na(p))) p <- as.POSIXct(res$last_seen, tz = "UTC")
        p
      }, error = function(e) {
        log_warn("data_telemetry",
                 "last_seen konnte nicht als Datum geparst werden: %s -- bleibt character.",
                 conditionMessage(e))
        NULL
      })
      if (!is.null(parsed) && !all(is.na(parsed)))
        res$last_seen <- parsed
    }

    attr(res, "status") <- status
  }

  res
}
