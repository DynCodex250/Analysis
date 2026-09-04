#' AOT-Metadaten aller Data Entities lesen
#'
#' Liest alle \code{AxDataEntity}-XML-Dateien aus dem lokalen AOT-Verzeichnis
#' und extrahiert Name, Modul und FormRef je Entity — ohne vollständiges
#' XML-Parsing, nur mittels Regex auf Rohtext (deutlich schneller).
#'
#' @details
#'
#' **Vorgehen (Ansatz C + A):**
#'
#' \enumerate{
#'   \item Alle \code{.xml}-Dateien unterhalb von \code{aot_root} werden aufgelistet.
#'   \item Gefiltert wird auf Dateien, deren Pfad den Ordner \code{AxDataEntity}
#'         enthält — das reduziert typischerweise von 10.000+ auf 800–1.500 Dateien.
#'   \item Jede Datei wird als Rohtext eingelesen (\code{readLines}).
#'         Drei Werte werden per Regex extrahiert:
#'         \itemize{
#'           \item \strong{Name}: Dateiname ohne \code{.xml}-Endung (technischer AOT-Name)
#'           \item \strong{Module}: Inhalt des Tags \code{<Module>...</Module>}
#'           \item \strong{FormRef}: Inhalt des Tags \code{<FormRef>...</FormRef>}
#'         }
#'   \item Tags, die in einer Datei nicht vorkommen, werden als \code{NA} zurückgegeben.
#' }
#'
#' @param aot_root Pfad zum lokalen AOT-Verzeichnis (PackagesLocalDirectory),
#'   z.B. \code{"C:/Users/.../PackagesLocalDirectory"}.
#'
#' @return Ein \code{data.frame} mit den Spalten:
#' \describe{
#'   \item{\code{Name}}{Technischer Entity-Name (AOT-Name, entspricht \code{TARGETENTITY} in SQL)}
#'   \item{\code{Module}}{Modul-Zuordnung aus dem XML-Tag \code{<Module>}}
#'   \item{\code{FormRef}}{Formular-/Menüpunkt-Referenz aus dem XML-Tag \code{<FormRef>};
#'     direkt verwendbar als \code{mi=}-Parameter in D365-Deeplinks}
#' }
#'
#' @examples
#' \dontrun{
#' aot_root <- "C:/Users/CKAM/AppData/Local/Microsoft/Dynamics365/10.0.2645.90/PackagesLocalDirectory"
#'
#' entity_meta <- get_entity_aot_metadata(aot_root)
#'
#' # Wie viele Entities haben eine FormRef?
#' sum(!is.na(entity_meta$FormRef))
#'
#' # Direkt als Mapping für D365-Deeplinks verwenden
#' entity_meta |>
#'   dplyr::filter(!is.na(FormRef)) |>
#'   dplyr::mutate(Name = tolower(Name))
#' }
#'
#' @export
get_entity_aot_metadata <- function(aot_root) {

  # --- Schritt C: Nur AxDataEntity-Dateien ---
  all_files <- list.files(
    path       = aot_root,
    pattern    = "\\.xml$",
    recursive  = TRUE,
    full.names = TRUE
  )

  # Windows nutzt \ als Trennzeichen — normalisieren für grepl
  all_files_norm <- gsub("\\\\", "/", all_files)
  entity_files   <- all_files[grepl("/AxDataEntity/", all_files_norm, fixed = TRUE)]

  if (length(entity_files) == 0) {
    warning("Keine AxDataEntity-XML-Dateien gefunden unter: ", aot_root)
    return(data.frame(
      Name    = character(),
      Module  = character(),
      FormRef = character(),
      stringsAsFactors = FALSE
    ))
  }

  message(sprintf("%d AxDataEntity-Dateien gefunden — lese...", length(entity_files)))

  # --- Hilfsfunktion: einen XML-Tag per Regex aus Rohtext extrahieren ---
  extract_tag <- function(txt, tag) {
    pattern <- paste0("(?<=<", tag, ">)[^<]+")
    m <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
    if (length(m) == 0L) NA_character_ else trimws(m)
  }

  # --- Schritt A: Rohtext einlesen + Regex ---
  results <- lapply(entity_files, function(f) {
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    list(
      Name    = tools::file_path_sans_ext(basename(f)),
      Module  = extract_tag(txt, "Module"),
      FormRef = extract_tag(txt, "FormRef")
    )
  })

  out <- data.frame(
    Name    = vapply(results, `[[`, character(1L), "Name"),
    Module  = vapply(results, `[[`, character(1L), "Module"),
    FormRef = vapply(results, `[[`, character(1L), "FormRef"),
    stringsAsFactors = FALSE
  )

  message(sprintf(
    "Fertig: %d Entities | mit Module: %d | mit FormRef: %d",
    nrow(out),
    sum(!is.na(out$Module)),
    sum(!is.na(out$FormRef))
  ))

  out
}
