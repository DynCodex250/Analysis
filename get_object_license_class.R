#' Lizenzklasse eines D365FO-Objekts ermitteln
#'
#' Bestimmt für ein oder mehrere AOT-Objekte die minimal erforderliche
#' Lizenz je Zugriffsebene (Read / Write).
#'
#' @details
#'
#' In D365FO kann dasselbe Objekt von mehreren Lizenzen abgedeckt werden.
#' Die tatsächlich ausgelöste Lizenz ist diejenige mit der niedrigsten
#' \code{Priority} (= günstigste Lizenz), die den benötigten
#' \code{ACCESSLEVEL} abdeckt:
#'
#' \itemize{
#'   \item \code{ACCESSLEVEL = 1} → Read-Zugriff
#'   \item \code{ACCESSLEVEL = 2} → Write-Zugriff
#' }
#'
#' Aus beiden Werten wird ein \code{license_delta} abgeleitet:
#'
#' \itemize{
#'   \item \code{"none"}    — Read und Write erfordern dieselbe Lizenz
#'   \item \code{"upgrade"} — Write erfordert eine höhere Lizenz als Read
#' }
#'
#' @param connection
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param aot_names
#' Character-Vektor mit einem oder mehreren AOT-Objektnamen
#' (z.B. \code{"BUDGETPLANLAYOUTDESCRIPTIONELEMENT"}).
#'
#' @return
#'
#' Tibble mit einer Zeile pro Objekt und Zugriffsebene.
#'
#' Spalten:
#'
#' \describe{
#'   \item{AOTNAME}{AOT-Name des Objekts.}
#'   \item{ACCESSLEVEL}{1 = Read, 2 = Write.}
#'   \item{min_priority}{Niedrigste Priority der abdeckenden Lizenzen.}
#'   \item{SKUNAME}{Name der ausgelösten Lizenz (niedrigste Priority).}
#'   \item{GROUPNAME}{Lizenzgruppe.}
#'   \item{license_delta}{
#'     \code{"none"} oder \code{"upgrade"} (nur bei ACCESSLEVEL 1 befüllt,
#'     zeigt ob Write eine höhere Lizenz als Read erfordert).
#'   }
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' result <- get_object_license_class(
#'   connection = cnn,
#'   aot_names  = c("BUDGETPLANLAYOUTDESCRIPTIONELEMENT", "PURCHTABLE")
#' )
#'
#' result
#'
#' # Nur Objekte, bei denen Write eine höhere Lizenz auslöst
#' dplyr::filter(result, license_delta == "upgrade")
#'
#' }
#'
#' @seealso
#' \code{\link{get_connection}}
#'
#' @export
get_object_license_class <- function(connection, aot_names) {

  #====================================================
  # SQL: alle Lizenz-Zeilen pro Objekt laden
  #====================================================

  placeholders <- paste(
    paste0("'", aot_names, "'"),
    collapse = ", "
  )

  sql <- paste0("
    SELECT
      obj.AOTNAME,
      laep.ACCESSLEVEL,
      COALESCE(lsku.[Priority], 10)          AS Priority,
      COALESCE(lsku.SKUNAME, 'Team Members') AS SKUNAME,
      lsku.GROUPNAME
    FROM       LicensingEntitlementObjects          obj
    LEFT JOIN  LicensingElementsRequiringEntitlement lere
               ON lere.ENTITLEMENTOBJECT = obj.RECID
    LEFT JOIN  LicensingAllEntitledPermissions       laep
               ON laep.ENTITLEMENTOBJECT = obj.RECID
    LEFT JOIN  LicensingAllSKUs                      lsku
               ON lsku.RECID = laep.SKURECID
    WHERE obj.AOTNAME IN (", placeholders, ")
  ")

  raw <- DBI::dbGetQuery(connection, sql)

  if (nrow(raw) == 0L) {
    return(tibble::tibble(
      AOTNAME       = character(),
      ACCESSLEVEL   = integer(),
      min_priority  = integer(),
      SKUNAME       = character(),
      GROUPNAME     = character(),
      license_delta = character()
    ))
  }

  #====================================================
  # Pro Objekt + ACCESSLEVEL: niedrigste Priority
  # = die tatsächlich ausgelöste Lizenz
  #====================================================

  classified <- raw |>
    dplyr::group_by(AOTNAME, ACCESSLEVEL) |>
    dplyr::slice_min(Priority, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(min_priority = Priority) |>
    dplyr::arrange(AOTNAME, ACCESSLEVEL)

  #====================================================
  # license_delta: steigt die Lizenz von Read → Write?
  #====================================================

  delta <- classified |>
    dplyr::select(AOTNAME, ACCESSLEVEL, min_priority) |>
    tidyr::complete(AOTNAME, ACCESSLEVEL = c(1L, 2L)) |>
    tidyr::pivot_wider(
      names_from   = ACCESSLEVEL,
      values_from  = min_priority,
      names_prefix = "level_"
    ) |>
    dplyr::mutate(
      license_delta = dplyr::case_when(
        is.na(.data$level_2)           ~ "read_only",
        .data$level_2 > .data$level_1  ~ "upgrade",
        TRUE                           ~ "none"
      )
    ) |>
    dplyr::select(AOTNAME, license_delta)

  classified |>
    dplyr::left_join(delta, by = "AOTNAME")
}
