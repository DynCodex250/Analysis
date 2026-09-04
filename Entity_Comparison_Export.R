# =============================================================================
# Entity Comparison Export
# =============================================================================
# Ziel:
#   1) Manuell vervollständigte Mapping-Tabelle als RData-Objekt speichern
#   2) EntityMonitoringResults aus SQL laden
#   3) Pivot-Vergleich zweier Legal Entities erstellen
#   4) Excel-Export mit klickbaren D365-Hyperlinks
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# 0) KONFIGURATION
# -----------------------------------------------------------------------------

# Pfad zur manuell vervollständigten Mapping-Datei (bitte anpassen)
mapping_file <- "C:/Users/ckam_admin/Documents/RProjects/D365_Admin/Data/FINAL_D365_comparison_new_.xlsx"
mapping_sheet <- "Comparison"

# Pfad zum RData-Objekt (wird erzeugt / geladen)
rdata_path <- "C:/Users/ckam_admin/Documents/RProjects/D365_Admin/Data/EntityMenuItemAssociation.RData"

# Pfad zum lokalen D365FO-AOT-Verzeichnis (für get_entity_aot_metadata)
aot_root <- "C:/Users/CKAM/AppData/Local/Microsoft/Dynamics365/10.0.2645.90/PackagesLocalDirectory"

# SQL-Verbindung
source("C:/Users/ckam_admin/Documents/RProjects/Funktionen_SQLConnection.R")
cred_path <- "C:\\Users\\ckam_admin\\Documents\\RProjects\\D365_Admin\\db_credentials.xlsx"
cnn_T <- get_connection("TST", cred_path)

# Legal Entities für den Vergleich
le_left  <- "0700"
le_right <- "0800"

# D365-Basis-URL
base_url <- "https://wibu-pri.sandbox.operations.eu.dynamics.com"

# Ausgabedatei
output_file <- paste0(
  "C:/Users/ckam_admin/Documents/RProjects/D365_Admin/Data/Entity_Comparison_",
  format(Sys.Date(), "%Y%m%d"), ".xlsx"
)

# =============================================================================
# SCHRITT 1: Mapping aus Excel laden und als RData speichern
# =============================================================================
# Ausführen wenn:
#   a) EntityMenuItemAssociation.RData noch nicht existiert, ODER
#   b) die Quelldatei manuell aktualisiert wurde

create_rdata <- !file.exists(rdata_path)

if (create_rdata) {

  raw <- readxl::read_excel(mapping_file, sheet = mapping_sheet)

  # Spaltennamen aus den Screenshots: A=TARGETENTITY, G=Module, I=MenuItem
  # Spaltenindizes als Fallback falls Namen abweichen
  col_names <- names(raw)

  entity_menu_mapping <- raw |>
    select(
      TARGETENTITY = 1,   # Spalte A: technischer Entity-Name
      Module       = 7,   # Spalte G: D365-Modul
      MenuItem     = 9    # Spalte I: Menu Item Name für D365-Deeplink
    ) |>
    filter(
      !is.na(TARGETENTITY),
      nchar(trimws(TARGETENTITY)) > 0
    ) |>
    mutate(
      TARGETENTITY = tolower(trimws(as.character(TARGETENTITY))),
      Module       = trimws(as.character(Module)),
      MenuItem     = trimws(as.character(MenuItem)),
      # NA normalisieren
      Module   = ifelse(Module   %in% c("NA", ""), NA_character_, Module),
      MenuItem = ifelse(MenuItem %in% c("NA", ""), NA_character_, MenuItem)
    ) |>
    # Pro Entity den ersten vollständigen Eintrag nehmen
    # (zuerst solche mit MenuItem bevorzugen)
    arrange(TARGETENTITY, is.na(MenuItem), is.na(Module)) |>
    group_by(TARGETENTITY) |>
    slice(1) |>
    ungroup()

  save(entity_menu_mapping, file = rdata_path)

  message(sprintf(
    "EntityMenuItemAssociation.RData erstellt: %d Entities, davon %d mit MenuItem",
    nrow(entity_menu_mapping),
    sum(!is.na(entity_menu_mapping$MenuItem))
  ))

} else {

  load(rdata_path)  # lädt entity_menu_mapping
  message(sprintf("EntityMenuItemAssociation.RData geladen: %d Entities", nrow(entity_menu_mapping)))

}

# =============================================================================
# SCHRITT 1b: AOT-Metadaten laden und Mapping anreichern
# =============================================================================
# get_entity_aot_metadata() liest alle AxDataEntity-XML-Dateien per Regex
# und liefert Name, Modules, FormRef. Fehlende Module/MenuItem im manuellen
# Mapping werden damit befüllt. Ergebnis: mergedEntityMenuItem.

EntityMenuItemAssociationFromAOT <- D365Security::get_entity_aot_metadata(aot_root) |>
  dplyr::mutate(
    Name    = tolower(trimws(as.character(Name))),
    Modules = trimws(as.character(Modules)),
    FormRef = trimws(as.character(FormRef)),
    Modules = ifelse(Modules %in% c("NA", ""), NA_character_, Modules),
    FormRef = ifelse(FormRef %in% c("NA", ""), NA_character_, FormRef)
  )

mergedEntityMenuItem <- entity_menu_mapping |>
  dplyr::left_join(
    EntityMenuItemAssociationFromAOT,
    by = c("TARGETENTITY" = "Name"),
    keep = TRUE
  ) |>
  dplyr::mutate(
    Module   = ifelse(is.na(Module),   Modules, Module),
    MenuItem = ifelse(is.na(MenuItem), FormRef, MenuItem)
  ) |>
  dplyr::select(TARGETENTITY, Module, MenuItem)

message(sprintf(
  "Mapping nach AOT-Anreicherung: %d Entities | mit Module: %d | mit MenuItem: %d",
  nrow(mergedEntityMenuItem),
  sum(!is.na(mergedEntityMenuItem$Module)),
  sum(!is.na(mergedEntityMenuItem$MenuItem))
))

# =============================================================================
# SCHRITT 2: EntityMonitoringResults aus SQL laden
# =============================================================================

monitoring_raw <- DBI::dbGetQuery(cnn_T, sprintf("
  SELECT
    LOWER(TargetEntity) AS TARGETENTITY,
    LOWER(EntityName)   AS ENTITYNAME,
    DataAreaId,
    RecordCount,
    RunDate
  FROM dbo.EntityMonitoringResults
  WHERE DataAreaId IN ('%s', '%s')
    AND Status = 'OK'
  ", le_left, le_right
))

message(sprintf(
  "EntityMonitoringResults: %d Zeilen (%s: %d / %s: %d)",
  nrow(monitoring_raw),
  le_left,  sum(monitoring_raw$DataAreaId == le_left),
  le_right, sum(monitoring_raw$DataAreaId == le_right)
))

# =============================================================================
# SCHRITT 3: Pivot – eine Zeile pro Entity, Spalten je Legal Entity
# =============================================================================

col_left  <- paste0("LE_", le_left)
col_right <- paste0("LE_", le_right)

monitoring_pivot <- monitoring_raw |>
  select(TARGETENTITY, ENTITYNAME, DataAreaId, RecordCount) |>
  # Bei mehrfachen Einträgen (z.B. mehrere RunDates) den letzten nehmen
  group_by(TARGETENTITY, ENTITYNAME, DataAreaId) |>
  summarise(RecordCount = last(RecordCount), .groups = "drop") |>
  pivot_wider(
    names_from  = DataAreaId,
    values_from = RecordCount,
    names_prefix = "LE_",
    values_fill = 0L
  ) |>
  mutate(
    Equal = .data[[col_left]] == .data[[col_right]],
    Diff  = .data[[col_right]] - .data[[col_left]]
  )

# =============================================================================
# SCHRITT 4: Join mit Mapping
# =============================================================================

comparison_data <- monitoring_pivot |>
  left_join(
    mergedEntityMenuItem |> select(TARGETENTITY, Module, MenuItem),
    by = "TARGETENTITY"
  ) |>
  arrange(Module, TARGETENTITY) |>
  select(
    Module,
    TARGETENTITY,
    ENTITYNAME,
    MenuItem,
    !!col_left,
    !!col_right,
    Diff,
    Equal
  )

message(sprintf(
  "Vergleichstabelle: %d Entities | Gleich: %d | Verschieden: %d",
  nrow(comparison_data),
  sum(comparison_data$Equal,  na.rm = TRUE),
  sum(!comparison_data$Equal, na.rm = TRUE)
))

# =============================================================================
# SCHRITT 5: Excel-Export mit Hyperlinks
# =============================================================================

build_hyperlink <- function(cmp, menu_item, display_value) {
  if (is.na(menu_item) || menu_item == "") {
    return(NA_character_)   # kein Link → writeData schreibt plain text
  }
  url <- paste0(base_url, "/?cmp=", cmp, "&mi=", menu_item)
  paste0('=HYPERLINK("', url, '","', display_value, '")')
}

export_entity_comparison <- function(data, file_name, le_left, le_right, base_url) {

  col_left  <- paste0("LE_", le_left)
  col_right <- paste0("LE_", le_right)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Comparison")

  # --- Styles ---
  style_header <- openxlsx::createStyle(
    fontColour = "#FFFFFF", bgFill = "#2F5496",
    textDecoration = "bold", halign = "center",
    border = "Bottom", borderColour = "#1F3864"
  )
  style_ok <- openxlsx::createStyle(
    fontColour = "#276221", bgFill = "#C6EFCE"
  )
  style_diff <- openxlsx::createStyle(
    fontColour = "#9C0006", bgFill = "#C5D9F1"
  )
  style_no_menu <- openxlsx::createStyle(
    fontColour = "#595959"   # grau = kein D365-Link vorhanden
  )
  style_number <- openxlsx::createStyle(
    numFmt = "#,##0", halign = "right"
  )
  style_link <- openxlsx::createStyle(
    fontColour = "#0563C1", textDecoration = "underline",
    numFmt = "#,##0", halign = "right"
  )

  # --- Header ---
  headers <- data.frame(
    Module       = "Module",
    TARGETENTITY = "TargetEntity",
    ENTITYNAME   = "EntityName",
    MenuItem     = "MenuItem",
    le_left_col  = paste0("LE_", le_left),
    le_right_col = paste0("LE_", le_right),
    Diff         = "Diff",
    Equal        = "Gleich?"
  )

  openxlsx::writeData(wb, "Comparison", headers, startRow = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "Comparison", style_header, rows = 1, cols = 1:8, gridExpand = TRUE)
  openxlsx::freezePane(wb, "Comparison", firstRow = TRUE)

  # --- Daten (ohne Hyperlink-Spalten, werden danach überschrieben) ---
  writeData(wb, "Comparison",
            data |> select(-!!col_left, -!!col_right),
            startRow = 2, colNames = FALSE, startCol = 1)

  # Platzhalterwerte für Hyperlink-Spalten schreiben (numerisch)
  writeData(wb, "Comparison",
            data |> select(!!col_left),
            startRow = 2, colNames = FALSE, startCol = 5)
  writeData(wb, "Comparison",
            data |> select(!!col_right),
            startRow = 2, colNames = FALSE, startCol = 6)

  # --- Hyperlinks zeilenweise schreiben ---
  for (i in seq_len(nrow(data))) {
    row_i <- i + 1
    mi    <- data$MenuItem[i]

    hl_left  <- build_hyperlink(le_left,  mi, data[[col_left]][i])
    hl_right <- build_hyperlink(le_right, mi, data[[col_right]][i])

    if (!is.na(hl_left)) {
      openxlsx::writeFormula(wb, "Comparison", x = hl_left,  xy = c(5, row_i))
      openxlsx::writeFormula(wb, "Comparison", x = hl_right, xy = c(6, row_i))
      openxlsx::addStyle(wb, "Comparison", style_link,
                         rows = row_i, cols = 5:6, gridExpand = TRUE, stack = TRUE)
    } else {
      openxlsx::addStyle(wb, "Comparison", style_no_menu,
                         rows = row_i, cols = 5:6, gridExpand = TRUE, stack = TRUE)
    }
  }

  # --- Bedingte Formatierung: Gleich/Verschieden ---
  rows_ok   <- which( data$Equal) + 1
  rows_diff <- which(!data$Equal) + 1

  if (length(rows_ok) > 0) {
    openxlsx::addStyle(wb, "Comparison", style_ok,
                       rows = rows_ok, cols = 8, gridExpand = TRUE, stack = TRUE)
  }
  if (length(rows_diff) > 0) {
    openxlsx::addStyle(wb, "Comparison", style_diff,
                       rows = rows_diff, cols = c(5,6,7,8),
                       gridExpand = TRUE, stack = TRUE)
  }

  # --- Zahlenformat für Count-Spalten ---
  openxlsx::addStyle(wb, "Comparison", style_number,
                     rows = 2:(nrow(data) + 1), cols = 5:7,
                     gridExpand = TRUE, stack = TRUE)

  # --- Spaltenbreiten ---
  openxlsx::setColWidths(wb, "Comparison",
                          cols   = 1:8,
                          widths = c(22, 38, 38, 35, 14, 14, 10, 9))

  # --- Tabellenformat (AutoFilter) ---
  openxlsx::addFilter(wb, "Comparison", rows = 1, cols = 1:8)

  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
  message(sprintf("Export: %s  (%d Zeilen)", file_name, nrow(data)))
}

export_entity_comparison(
  data      = comparison_data,
  file_name = output_file,
  le_left   = le_left,
  le_right  = le_right,
  base_url  = base_url
)
