#' Benutzerzuweisungen von Standardrollen auf WIBU-Klone umstellen
#'
#' Lädt alle Benutzerzuweisungen auf die im Mapping enthaltenen Standardrollen,
#' ersetzt diese durch die entsprechenden WIBU-Klone und gibt das Ergebnis
#' im D365-Importformat zurück — optional als Excel-Datei.
#'
#' @details
#'
#' **Ablauf**
#'
#' \enumerate{
#'   \item Benutzerzuweisungen für alle Standardrollen aus der Mapping-Datei
#'         laden (\code{get_sql_user_role_assignments()}).
#'   \item AOT-Namen der Zielrollen (WIBU-Klone) per Namens-Lookup aus der DB
#'         ermitteln, da die Mapping-Datei nur den Anzeigenamen enthält.
#'   \item Zuweisungen per Mapping-Join ersetzen.
#'   \item Nicht zuordenbare Zuweisungen separat ausweisen.
#' }
#'
#' **Mapping-Tabelle**
#'
#' Das \code{role_map}-Argument ist typischerweise die projektspezifische
#' Excel-Mapping-Datei, die für jede Standardrolle den entsprechenden
#' WIBU-Klon ausweist:
#'
#' \preformatted{
#' role_map <- readxl::read_excel(mapping_pfad) |>
#'   dplyr::mutate(Bezeichner = toupper(Bezeichner))
#' }
#'
#' Erwartete Spalten (Standardnamen, über \code{from_col}/\code{to_name_col}
#' anpassbar):
#'
#' \describe{
#'   \item{\code{Bezeichner}}{AOT-Name der Standardrolle (FROM).}
#'   \item{\code{WibuRolle}}{Anzeigename des WIBU-Klons (TO). Der AOT-Name
#'         wird automatisch per DB-Lookup ermittelt.}
#' }
#'
#' **Unterschied zu \code{sec_migrate_ui_roles_to_aot()}}
#'
#' \code{sec_migrate_ui_roles_to_aot()} ersetzt UI-Rollen-GUIDs durch
#' AOT-Identifier. Diese Funktion arbeitet AOT-zu-AOT: Standardrolle
#' (AOT-Name) → WIBU-Klon (AOT-Name).
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param role_map
#' Tibble mit dem Mapping Standardrolle → WIBU-Klon.
#' Muss mindestens die Spalten \code{from_col} (AOT-Name der Standardrolle)
#' und \code{to_name_col} (Anzeigename des WIBU-Klons) enthalten.
#'
#' @param from_col
#' Name der Spalte in \code{role_map}, die den AOT-Identifier der
#' Standardrolle enthält. Standard: \code{"Bezeichner"}.
#'
#' @param to_name_col
#' Name der Spalte in \code{role_map}, die den Anzeigenamen des
#' WIBU-Klons enthält. Standard: \code{"WibuRolle"}.
#'
#' @param user_ids
#' Optionaler Character-Vektor mit Benutzer-IDs.
#' \code{NULL} = alle Benutzer.
#'
#' @param output_file
#' Optionaler Dateipfad (\code{.xlsx}) für den Excel-Export.
#' \code{NULL} = kein Export.
#'
#' @return
#'
#' Benannte Liste mit zwei Tibbles:
#'
#' \describe{
#'   \item{\code{$export}}{
#'     Zuweisungen mit WIBU-Klonen — bereit für den D365-Import.
#'
#'     Spalten: \code{USERID}, \code{SECURITYROLEIDENTIFIER} (AOT-Name
#'     des WIBU-Klons), \code{SECURITYROLENAME} (Anzeigename), \code{ORGANIZATIONID}.
#'   }
#'   \item{\code{$unmatched}}{
#'     Zuweisungen, für die kein Mapping gefunden wurde — manuell prüfen.
#'
#'     Spalten: wie \code{get_sql_user_role_assignments()}.
#'   }
#' }
#'
#' Wenn \code{output_file} angegeben ist, wird eine Excel-Datei mit zwei
#' Blättern erzeugt: \code{"D365 Import"} und \code{"Unmatched (pruefen)"}.
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' role_map <- readxl::read_excel(
#'   "_WIBU Stammdaten Kunden Manager/Standardrolle-Duty - Masterdaten von Debitoren verwalten.xlsx"
#' ) |>
#'   dplyr::mutate(Bezeichner = toupper(Bezeichner))
#'
#' # Alle betroffenen Benutzer migrieren
#' ergebnis <- sec_swap_role_assignments(cnn, role_map)
#' ergebnis$export     # bereit für D365-Import
#' ergebnis$unmatched  # kein Mapping → manuell prüfen
#'
#' # Nur bestimmte Benutzer, direkt als Excel exportieren
#' sec_swap_role_assignments(
#'   con         = cnn,
#'   role_map    = role_map,
#'   user_ids    = c("wibule01", "wibule02"),
#'   output_file = "C:/Output/zielzuweisungen.xlsx"
#' )
#'
#' # Andere Spaltennamen in der Mapping-Datei
#' sec_swap_role_assignments(
#'   con          = cnn,
#'   role_map     = mein_mapping,
#'   from_col     = "StandardrolleAOT",
#'   to_name_col  = "WibuKlonName"
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{get_sql_user_role_assignments}}
#' \code{\link{sec_migrate_ui_roles_to_aot}}
#'
#' @export
sec_swap_role_assignments <- function(
  con,
  role_map,
  from_col     = "Bezeichner",
  to_name_col  = "WibuRolle",
  user_ids     = NULL,
  output_file  = NULL
) {

  #====================================================
  # Mapping vorbereiten
  #====================================================
  mapping_slim <- role_map |>
    dplyr::select(
      FROM_IDENTIFIER = dplyr::all_of(from_col),
      TO_NAME         = dplyr::all_of(to_name_col)
    ) |>
    dplyr::distinct()

  from_identifiers <- unique(mapping_slim[["FROM_IDENTIFIER"]])
  to_names         <- unique(mapping_slim[["TO_NAME"]])

  #====================================================
  # AOT-Namen der Zielrollen per DB-Lookup ermitteln
  # (Mapping-Datei enthält nur Anzeigenamen)
  #====================================================
  to_names_sql <- paste0(
    "'", gsub("'", "''", to_names), "'", collapse = ", "
  )

  wibu_aot <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT AOTNAME AS TO_IDENTIFIER, NAME AS TO_NAME ",
      "FROM SECURITYROLE ",
      "WHERE NAME IN (", to_names_sql, ")"
    )
  ) |> tibble::as_tibble()

  # Vollständiges Mapping: FROM_IDENTIFIER → TO_IDENTIFIER + TO_NAME
  mapping_full <- mapping_slim |>
    dplyr::inner_join(wibu_aot, by = "TO_NAME")

  #====================================================
  # Aktuelle Zuweisungen für die Standardrollen laden
  #====================================================
  assignments <- get_sql_user_role_assignments(
    con      = con,
    roles    = from_identifiers,
    user_ids = if (is.null(user_ids)) character() else user_ids
  )

  #====================================================
  # Zielzuweisungen (Standardrolle → WIBU-Klon)
  #====================================================
  export <- assignments |>
    dplyr::inner_join(
      mapping_full,
      by = c("SECURITYROLEIDENTIFIER" = "FROM_IDENTIFIER")
    ) |>
    dplyr::transmute(
      USERID,
      SECURITYROLEIDENTIFIER = TO_IDENTIFIER,
      SECURITYROLENAME       = TO_NAME,
      ORGANIZATIONID
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(USERID, SECURITYROLEIDENTIFIER, ORGANIZATIONID)

  #====================================================
  # Nicht zuordenbare Zuweisungen
  #====================================================
  unmatched <- assignments |>
    dplyr::anti_join(
      mapping_full,
      by = c("SECURITYROLEIDENTIFIER" = "FROM_IDENTIFIER")
    ) |>
    dplyr::arrange(USERID, SECURITYROLEIDENTIFIER)

  #====================================================
  # Optional: Excel-Export
  #====================================================
  if (!is.null(output_file)) {
    writexl::write_xlsx(
      list(
        `D365 Import`         = export,
        `Unmatched (pruefen)` = unmatched
      ),
      output_file
    )
    message(
      "Export gespeichert: ", output_file, "\n",
      "  Zeilen Export:     ", nrow(export), "\n",
      "  Zeilen Unmatched:  ", nrow(unmatched)
    )
  }

  list(
    export    = export,
    unmatched = unmatched
  )
}
