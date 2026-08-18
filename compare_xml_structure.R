# Interne Hilfsfunktion: XML-Struktur eines geladenen Dokuments extrahieren.
#
# Gibt alle eindeutigen Element-Pfade zurück, z. B.:
#   "AxSecurityRole"
#   "AxSecurityRole/Name"
#   "AxSecurityRole/Duties/AxSecurityDutyReference"
#
# Text-Inhalte der Knoten werden ignoriert.
# Attribute werden nicht berücksichtigt.
#
# Verwendet xml2::xml_path() statt manuellem Parent-Traversal,
# um den Fehler "Parent does not exist" zu vermeiden.
extract_xml_structure <- function(doc) {

  all_nodes <- xml2::xml_find_all(doc, "//*")

  if (length(all_nodes) == 0L) {
    return(character(0))
  }

  paths <- vapply(all_nodes, function(node) {
    # xml_path() gibt z.B. "/AxSecurityRole/Duties/AxSecurityDutyReference[1]/Name[1]"
    raw <- xml2::xml_path(node)
    # XPath-Indizes [n] entfernen, führenden Slash abschneiden
    raw <- gsub("\\[\\d+\\]", "", raw)
    raw <- sub("^/", "", raw)
    raw
  }, character(1))

  unique(sort(paths))
}


#' Zwei XML-Dateien auf Strukturgleichheit prüfen
#'
#' Vergleicht die hierarchische Elementstruktur zweier XML-Dateien.
#'
#' Die Funktion prüft, ob beide Dateien dieselben XML-Elementtypen
#' in derselben hierarchischen Anordnung enthalten.
#'
#' Verglichen werden ausschließlich:
#'
#' * Elementnamen
#' * Hierarchische Position (Pfad von der Wurzel bis zum Element)
#'
#' Nicht verglichen werden:
#'
#' * Textwerte der Elemente (z.B. Rollenname, Duty-Name)
#' * Attribute
#' * Reihenfolge gleichrangiger Elemente
#' * Leerzeichen und Zeilenumbrüche
#'
#' @details
#'
#' Zwei Elemente mit unterschiedlichem Inhalt, aber identischem Pfad
#' werden als strukturell gleich betrachtet:
#'
#' \preformatted{
#' <RoleName>RoleA</RoleName>   }
#' <RoleName>RoleB</RoleName>   } beide -> "AxSecurityRole/RoleName"
#' }
#'
#' Die Funktion eignet sich insbesondere für:
#'
#' * Prüfung vor einem fachlichen Security-Vergleich
#' * Erkennung unerwarteter XML-Sektionen
#' * Regressionstests nach XML-Modifikationen
#'
#' @param xml_file_1
#' Pfad zur ersten XML-Datei.
#'
#' @param xml_file_2
#' Pfad zur zweiten XML-Datei.
#'
#' @return
#'
#' Liste mit drei Elementen:
#'
#' \describe{
#'
#' \item{identical}{
#' \code{TRUE} wenn beide Dateien dieselbe Struktur besitzen,
#' sonst \code{FALSE}.
#' }
#'
#' \item{only_in_file_1}{
#' Tibble mit Spalte \code{element_path}: Element-Pfade, die nur in
#' Datei 1 vorhanden sind (z.B. \code{"AxSecurityRole/Duties/AxSecurityDutyReference"}).
#' Leer-Tibble wenn keine Unterschiede.
#' }
#'
#' \item{only_in_file_2}{
#' Tibble mit Spalte \code{element_path}: Element-Pfade, die nur in
#' Datei 2 vorhanden sind.
#' Leer-Tibble wenn keine Unterschiede.
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' result <- compare_xml_structure(
#'   xml_file_1 = "AxSecurityRole_Standard.xml",
#'   xml_file_2 = "AxSecurityRole__WIBU.xml"
#' )
#'
#' result$identical
#'
#' result$only_in_file_1
#'
#' result$only_in_file_2
#'
#' }
#'
#' @seealso
#' \code{\link{compare_security_roles}}
#'
#' @export
compare_xml_structure <- function(
    xml_file_1,
    xml_file_2) {

  #====================================================
  # XML LADEN
  #====================================================

  doc1 <- tryCatch(
    xml2::read_xml(xml_file_1),
    error = function(e) {
      stop(paste0(
        "XML-Datei 1 konnte nicht geladen werden: ",
        xml_file_1, "\n",
        conditionMessage(e)
      ))
    }
  )

  doc2 <- tryCatch(
    xml2::read_xml(xml_file_2),
    error = function(e) {
      stop(paste0(
        "XML-Datei 2 konnte nicht geladen werden: ",
        xml_file_2, "\n",
        conditionMessage(e)
      ))
    }
  )

  #====================================================
  # STRUKTUR EXTRAHIEREN
  #====================================================

  struct1 <- extract_xml_structure(doc1)
  struct2 <- extract_xml_structure(doc2)

  #====================================================
  # VERGLEICH
  #====================================================

  only_in_1 <- setdiff(struct1, struct2)
  only_in_2 <- setdiff(struct2, struct1)

  list(
    identical      = length(only_in_1) == 0L && length(only_in_2) == 0L,
    only_in_file_1 = tibble::tibble(element_path = only_in_1),
    only_in_file_2 = tibble::tibble(element_path = only_in_2)
  )
}
