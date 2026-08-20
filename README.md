#' Lizenzen je Rolle kompakt (eine Zeile pro Rolle)
#'
#' Wrapper um \code{get_role_license_assignments()}, der alle SKUs
#' einer Rolle in einer einzigen Zeile zusammenfasst (pipe-separiert).
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param role_identifiers
#' Optionaler Character-Vektor mit Rollen-AOTNAMEs.
#' Wenn leer, werden alle Rollen zurückgegeben.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Rolle.
#'
#' \describe{
#'   \item{ROLEIDENTIFIER}{AOT-Name der Rolle.}
#'   \item{SECURITYROLENAME}{Anzeigename der Rolle.}
#'   \item{SKUNAME}{Alle Lizenz-SKUs, pipe-separiert (z.B. \code{"Finance | Commerce"}).}
#'   \item{SKURECID}{Alle internen Lizenz-IDs, pipe-separiert.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' # Alle Rollen kompakt
#' get_role_license_assignments_compact(cnn)
#'
#' # Nur bestimmte Rollen
#' get_role_license_assignments_compact(
#'   cnn,
#'   role_identifiers = c("_WIBU_VERKAUF_KEYACCOUNT", "_WIBU_EINKAUF_MITARBEITER")
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{get_role_license_assignments}}
#'
#' @export
get_role_license_assignments_compact <- function(
  con,
  role_identifiers = character()
) {
  get_role_license_assignments(con, role_identifiers = role_identifiers) |>
    dplyr::group_by(ROLEIDENTIFIER, SECURITYROLENAME) |>
    dplyr::summarise(
      SKUNAME  = paste(SKUNAME,  collapse = " | "),
      SKURECID = paste(SKURECID, collapse = " | "),
      .groups  = "drop"
    ) |>
    dplyr::arrange(ROLEIDENTIFIER)
}
