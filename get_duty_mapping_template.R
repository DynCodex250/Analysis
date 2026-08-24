#' Mapping-Template für Duty-Zentralisierung erstellen
#'
#' Generiert eine Vorlage für die projektspezifische Mapping-Datei, die
#' bei der Duty-Zentralisierung (\code{sec_swap_role_assignments()}) benötigt wird.
#'
#' Alle Standardrollen, die die angegebene Duty direkt oder über eine Subrolle
#' enthalten, werden aus der DB ermittelt. Das Ergebnis enthält bereits
#' AOT-Name, Anzeigename und Beschreibung — die Spalten \code{WibuRolle} und
#' \code{Abteilung} müssen manuell ergänzt werden.
#'
#' @details
#'
#' **Typischer Ablauf**
#'
#' \preformatted{
#' # 1. Template generieren
#' template <- get_duty_mapping_template(cnn, "CustCustomersMaintain")
#'
#' # 2. WibuRolle + Abteilung manuell ergänzen (z.B. in Excel öffnen)
#' writexl::write_xlsx(template, "mapping_template.xlsx")
#'
#' # 3. Ausgefüllte Datei wieder einlesen
#' role_map <- readxl::read_excel("mapping_fertig.xlsx") |>
#'   dplyr::mutate(Bezeichner = toupper(Bezeichner))
#' }
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param duty_identifier
#' AOT-Name der Duty (z.B. \code{"CustCustomersMaintain"}).
#'
#' @return
#'
#' Tibble mit einer Zeile pro betroffener Standardrolle:
#'
#' \describe{
#'   \item{\code{Bezeichner}}{AOT-Name der Standardrolle (JOIN-Schlüssel für \code{sec_swap_role_assignments()}).}
#'   \item{\code{Standardrolle}}{Anzeigename (aus DB).}
#'   \item{\code{Beschreibung}}{Rollenbeschreibung aus \code{SECURITYROLE} (leer wenn nicht vorhanden).}
#'   \item{\code{WibuRolle}}{Leer — manuell mit dem Anzeigenamen des WIBU-Klons zu befüllen.}
#'   \item{\code{Abteilung}}{Leer — optional manuell zu befüllen.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' # Template für CustCustomersMaintain generieren
#' template <- get_duty_mapping_template(cnn, "CustCustomersMaintain")
#'
#' # Als Excel exportieren → manuell mit WibuRolle-Spalte befüllen
#' writexl::write_xlsx(
#'   template,
#'   "Standardrolle-Duty - Masterdaten von Debitoren verwalten_TEMPLATE.xlsx"
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{sec_swap_role_assignments}}
#' \code{\link{get_sql_security_hierarchy}}
#' \code{\link{sec_get_direct_access_roles}}
#'
#' @export
get_duty_mapping_template <- function(con, duty_identifier) {

  #====================================================
  # Hierarchie laden und Standard-Rollen ermitteln
  #====================================================
  hierarchie <- get_sql_security_hierarchy(
    con              = con,
    duty_identifiers = duty_identifier
  )

  zugang <- sec_get_direct_access_roles(hierarchie)

  # Alle direkten Träger + Subrollen zusammenführen
  alle_roles <- dplyr::bind_rows(
    zugang$direkt |>
      dplyr::distinct(ROLEIDENTIFIER, ROLENAME),
    zugang$via_subrole |>
      dplyr::distinct(
        ROLEIDENTIFIER = SUBROLEIDENTIFIER,
        ROLENAME       = SUBROLENAME
      )
  ) |>
    dplyr::filter(!grepl("wibu", ROLENAME, ignore.case = TRUE)) |>
    dplyr::distinct(ROLEIDENTIFIER, ROLENAME) |>
    dplyr::arrange(ROLEIDENTIFIER)

  if (nrow(alle_roles) == 0L) {
    message("Keine Standardrollen mit Duty '", duty_identifier, "' gefunden.")
    return(tibble::tibble(
      Bezeichner    = character(),
      Standardrolle = character(),
      Beschreibung  = character(),
      WibuRolle     = character(),
      Abteilung     = character()
    ))
  }

  #====================================================
  # Beschreibungen aus SECURITYROLE nachschlagen
  #====================================================
  ids_sql <- paste0(
    "'", gsub("'", "''", alle_roles[["ROLEIDENTIFIER"]]), "'",
    collapse = ", "
  )

  beschreibungen <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT AOTNAME AS ROLEIDENTIFIER, DESCRIPTION AS Beschreibung ",
      "FROM SECURITYROLE ",
      "WHERE AOTNAME IN (", ids_sql, ")"
    )
  ) |> tibble::as_tibble()

  #====================================================
  # Template zusammenbauen
  #====================================================
  alle_roles |>
    dplyr::left_join(beschreibungen, by = "ROLEIDENTIFIER") |>
    dplyr::transmute(
      Bezeichner    = ROLEIDENTIFIER,
      Standardrolle = ROLENAME,
      Beschreibung  = dplyr::coalesce(Beschreibung, ""),
      WibuRolle     = "",
      Abteilung     = ""
    )
}
