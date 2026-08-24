#' Standardrolle klonen und Duties / Privileges entfernen
#'
#' Liest die AOT-XML einer D365FO-Standardrolle, erstellt eine Kopie mit neuem
#' Namen und entfernt dabei gezielt direkt zugewiesene Duties und/oder
#' Privileges. SubRoles bleiben unberührt.
#'
#' @details
#'
#' **Typischer Anwendungsfall**
#'
#' Duty-Zentralisierung: Eine Standardrolle soll durch einen WIBU-Klon ersetzt
#' werden, der dieselben Rechte hat — aber ohne die Duties/Privileges, die
#' fortan ausschließlich über eine dedizierte Zusatzrolle vergeben werden.
#'
#' \preformatted{
#' clone_role_without(
#'   aot_root          = "C:/Users/CKAM/AppData/Local/.../PackagesLocalDirectory",
#'   role_identifier   = "COLLECTIONLETTERCOLLECTIONSAGENT",
#'   new_name          = "_WIBU_FiBu_Inkassobeauftragter",
#'   new_label         = "_WIBU FiBu Inkassobeauftragter",
#'   remove_duties     = "CustCustomersMaintain",
#'   remove_privileges = character(),
#'   output_dir        = "output/"
#' )
#' }
#'
#' **Was wird entfernt?**
#'
#' Aus den Abschnitten `<Duties>` und `<Privileges>` der geklonten XML werden
#' alle `AxSecurityDutyReference`- bzw. `AxSecurityPrivilegeReference`-Knoten
#' entfernt, deren `<Name>` in `remove_duties` bzw. `remove_privileges` steht.
#' Die Suche ist case-insensitive. SubRoles (`<SubRoles>`) werden nicht
#' verändert.
#'
#' **Ausgabe**
#'
#' Die neue XML-Datei heißt `<new_name>.xml` und wird in `output_dir`
#' geschrieben. Sie kann direkt als AOT-Objekt importiert werden.
#'
#' @param aot_root
#' Pfad zum `PackagesLocalDirectory` des lokalen D365FO-Builds.
#'
#' @param role_identifier
#' AOT-Name der zu klonenden Standardrolle (z.B.
#' `"COLLECTIONLETTERCOLLECTIONSAGENT"`).
#'
#' @param new_name
#' AOT-Name des neuen WIBU-Klons. Wird als Dateiname und als `<Name>`-Element
#' in der XML verwendet.
#'
#' @param new_label
#' Anzeigename (Label) des neuen Klons. Wird als `<Label>`-Element in die XML
#' geschrieben. \code{NULL} = Label der Originalrolle beibehalten.
#'
#' @param remove_duties
#' Character-Vektor mit AOT-Namen der Duties, die aus der geklonten Rolle
#' entfernt werden sollen. Leerer Vektor = keine Duty entfernen.
#'
#' @param remove_privileges
#' Character-Vektor mit AOT-Namen der Privileges, die direkt aus der geklonten
#' Rolle entfernt werden sollen. Leerer Vektor = kein Privilege entfernen.
#'
#' @param output_dir
#' Ausgabeverzeichnis für die neue XML-Datei. Wird angelegt, falls nicht
#' vorhanden.
#'
#' @return
#' Unsichtbar: vollständiger Pfad der geschriebenen XML-Datei.
#'
#' @examples
#' \dontrun{
#'
#' aot <- "C:/Users/CKAM/AppData/Local/Microsoft/Dynamics365/10.0.2645.90/PackagesLocalDirectory"
#'
#' # Rolle klonen und eine Duty entfernen
#' clone_role_without(
#'   aot_root          = aot,
#'   role_identifier   = "COLLECTIONLETTERCOLLECTIONSAGENT",
#'   new_name          = "_WIBU_FiBu_Inkassobeauftragter",
#'   new_label         = "_WIBU FiBu Inkassobeauftragter",
#'   remove_duties     = "CustCustomersMaintain",
#'   output_dir        = "output/SecurityRole"
#' )
#'
#' # Mehrere Duties und ein Privilege entfernen
#' clone_role_without(
#'   aot_root          = aot,
#'   role_identifier   = "TRADESALESMANAGER",
#'   new_name          = "_WIBU_Verkauf_Verkaufsleiter",
#'   new_label         = "_WIBU Verkauf Verkaufsleiter",
#'   remove_duties     = c("CustCustomersMaintain", "SomeOtherDuty"),
#'   remove_privileges = "SomeDirectPrivilege",
#'   output_dir        = "output/SecurityRole"
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{find_security_object_file}}
#' \code{\link{sec_swap_role_assignments}}
#' \code{\link{get_duty_mapping_template}}
#'
#' @export
clone_role_without <- function(
  aot_root,
  role_identifier,
  new_name,
  new_label         = NULL,
  remove_duties     = character(),
  remove_privileges = character(),
  output_dir
) {

  #====================================================
  # XML-Quelldatei finden
  #====================================================
  xml_path <- find_security_object_file(
    object_name = role_identifier,
    category    = "Role",
    root_folder = aot_root
  )

  if (is.null(xml_path)) {
    stop("Rolle '", role_identifier, "' nicht im AOT gefunden: ", aot_root)
  }

  if (length(xml_path) > 1) {
    message("Mehrere Treffer fuer '", role_identifier, "' — erster Treffer wird verwendet:\n  ", xml_path[1])
    xml_path <- xml_path[1]
  }

  #====================================================
  # XML lesen und unabhängige Kopie erstellen
  # (xml2 hat keine public clone()-Funktion;
  #  Round-trip über as.character ist der portable Weg)
  #====================================================
  original <- xml2::read_xml(xml_path)
  doc      <- xml2::read_xml(as.character(original))

  #====================================================
  # Name setzen
  #====================================================
  name_node <- xml2::xml_find_first(doc, "/AxSecurityRole/Name")
  if (!is.na(name_node)) {
    xml2::xml_set_text(name_node, new_name)
  } else {
    stop("Kein <Name>-Element in der XML der Rolle '", role_identifier, "' gefunden.")
  }

  #====================================================
  # Label setzen (wenn angegeben)
  #====================================================
  if (!is.null(new_label)) {
    label_node <- xml2::xml_find_first(doc, "/AxSecurityRole/Label")
    if (!is.na(label_node)) {
      xml2::xml_set_text(label_node, new_label)
    }
  }

  #====================================================
  # Duties entfernen
  #====================================================
  if (length(remove_duties) > 0) {
    duty_refs <- xml2::xml_find_all(
      doc,
      "/AxSecurityRole/Duties/AxSecurityDutyReference"
    )
    for (ref in duty_refs) {
      ref_name <- xml2::xml_text(xml2::xml_find_first(ref, "Name"))
      if (!is.na(ref_name) && tolower(ref_name) %in% tolower(remove_duties)) {
        xml2::xml_remove(ref)
      }
    }
  }

  #====================================================
  # Privileges entfernen (nur direkt zugewiesene)
  #====================================================
  if (length(remove_privileges) > 0) {
    priv_refs <- xml2::xml_find_all(
      doc,
      "/AxSecurityRole/Privileges/AxSecurityPrivilegeReference"
    )
    for (ref in priv_refs) {
      ref_name <- xml2::xml_text(xml2::xml_find_first(ref, "Name"))
      if (!is.na(ref_name) && tolower(ref_name) %in% tolower(remove_privileges)) {
        xml2::xml_remove(ref)
      }
    }
  }

  #====================================================
  # XML schreiben
  #====================================================
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_path <- file.path(output_dir, paste0(new_name, ".xml"))
  xml2::write_xml(doc, output_path, options = "format")

  removed_duties_found <- if (length(remove_duties) > 0) {
    paste(remove_duties, collapse = ", ")
  } else {
    "keine"
  }
  removed_privs_found <- if (length(remove_privileges) > 0) {
    paste(remove_privileges, collapse = ", ")
  } else {
    "keine"
  }

  message(
    "Klon erstellt: ", output_path, "\n",
    "  Original:            ", role_identifier, "\n",
    "  Neuer AOT-Name:      ", new_name, "\n",
    "  Entfernte Duties:    ", removed_duties_found, "\n",
    "  Entfernte Privileges:", removed_privs_found
  )

  invisible(output_path)
}
