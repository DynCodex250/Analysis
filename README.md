---
title: "Rechnungs-E-Mail: Analyse fehlerhafter Verwendungszwecke"
subtitle: "Analysebericht – D365FO Systemadministration"
author: "Cedric Kameni"
date: "`r format(Sys.Date(), '%d. %B %Y')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float:
      collapsed: false
    number_sections: false
    df_print: paged
  word_document:
    toc: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

cred_path <- ""
cnn <- D365Security::get_connection("TST", cred_path)

library(dplyr)
library(knitr)
library(ggplot2)
library(openxlsx)
```

---

> 📦 **Datengrundlage: Kontaktdaten aus PROD-Umgebung**
> Die Partei-Kontaktdaten (`PartyContactsExport`) wurden aus der **PROD-Umgebung** exportiert
> und in die TST-Datenbank geladen, da Microsoft in Testumgebungen alle E-Mail-Adressen
> aus Datenschutzgründen entfernt. Alle Analyseergebnisse basieren auf echten PROD-Daten.

---

## Datengrundlage

### Warum nicht direkt aus TST?

Microsoft entfernt in Sandbox- und Testumgebungen (TST) systematisch alle persönlichen Kontaktdaten aus `DIRPARTYCONTACTENTITY`. Eine direkte Analyse gegen TST würde daher leere Ergebnisse liefern.

### Prozess: PROD-Daten in TST laden

Um dennoch eine valide Analyse durchzuführen, werden die Party-Kontaktdaten aus der PROD-Umgebung exportiert und als eigenständige Tabelle in TST bereitgestellt:

**Schritt 1 — Export aus PROD (Dynamics 365 Data Management):**

- Entität: **Party contacts V3** (`DIRPARTYCONTACTENTITY`)
- Format: Excel (`.xlsx`)
- Umgebung: PROD

**Schritt 2 — Import in TST-Datenbank (R):**

```r
# TYPE-Mapping: D365-UI-Export liefert TYPE als Textwert ("Email", "Phone" etc.)
# → muss in die entsprechenden Enum-Integerwerte der Datenbank konvertiert werden
type_mapping <- c(
  "Phone" = 1L,
  "Email" = 2L,
  "URL"   = 3L,
  "Fax"   = 5L
)

# PROD-Export einlesen und TYPE konvertieren
PartyContactsExport <- readxl::read_excel(
  "C:/Users/CKAM/Documents/RProjects/PartyContactsExport.xlsx"
) |>
  mutate(
    TYPE = unname(type_mapping[TYPE]),
    TYPE = as.integer(TYPE)
  )

# Qualitätsprüfung: unbekannte TYPE-Werte prüfen (Ergebnis sollte 0 Zeilen sein)
PartyContactsExport |> filter(is.na(TYPE))

# In TST-SQL-Server schreiben
odbc::dbWriteTable(
  cnn,
  name      = "PartyContactsExport",
  value     = PartyContactsExport,
  overwrite = TRUE
)
```

> **Warum TYPE konvertieren?** Der D365-Datenmanagement-Export gibt den Typ als lesbaren Text aus (`"Email"`, `"Phone"` etc.). Die SQL-Abfragen filtern jedoch auf den numerischen Enum-Wert (`TYPE = 2` für E-Mail). Ohne diese Konvertierung würden alle TYPE-Filter keine Treffer liefern.

Die Tabelle `PartyContactsExport` steht danach in TST zur Verfügung und enthält die echten PROD-Kontaktdaten. Sie ersetzt in allen nachfolgenden Abfragen die systemseitige `DIRPARTYCONTACTENTITY`.

> ⚠️ **Hinweis:** Dieser Schritt muss vor dem Knitten dieses Dokuments manuell ausgeführt werden, wenn neue PROD-Daten benötigt werden.

---

## Hintergrund

Ein Kunde hat keine Rechnung per E-Mail erhalten, obwohl eine E-Mail-Adresse im System hinterlegt war. Die Ursache: Der hinterlegte **Verwendungszweck** (`PURPOSE`) lautete `eInvoice` — ohne Gesellschaftssuffix.

Die Druckverwaltung (Print Management) in D365FO sucht beim Rechnungsversand nach dem **exakten gesellschaftsspezifischen Wert** (z.B. `eInvoice_0900`). Ein generisches `eInvoice` wird nie gefunden — die Rechnung wird nicht versandt.

Ziel dieser Analyse: Alle betroffenen Kunden identifizieren, nach Schweregrad klassifizieren und konkrete Maßnahmen ableiten.

> **Abgrenzung:** Intercompany-Kunden (`CUSTGROUP LIKE '%IC'`) sind in allen Abfragen ausgeschlossen. Ein fehlender Purpose ist bei IC-Kunden kein Fehler — der IC-Rechnungsprozess läuft über eigene Logik. *(Entscheidung steht formal noch aus — siehe [Offene Punkte](#offene-punkte).)*

### Was bedeutet das in der Praxis?

Stellen Sie sich vor, das System hat für einen Kunden die E-Mail-Adresse `rechnung@beispiel.de` gespeichert — und dennoch erhält der Kunde keine Rechnung per E-Mail. Der Grund liegt nicht in der E-Mail-Adresse selbst, sondern in einem **unsichtbaren Kennzeichen** (dem sogenannten Verwendungszweck), das dieser E-Mail-Adresse zugeordnet ist.

D365FO arbeitet mit mehreren rechtlichen Einheiten — in unserem Fall die Gesellschaften 0100, 0360, 0500, 0600, 0700, 0800 und 0900. Beim automatischen Rechnungsversand sucht das System explizit nach einer E-Mail-Adresse mit dem Zweck `eInvoice_0900` (Beispiel für Gesellschaft 0900). Lautet der gespeicherte Zweck stattdessen nur `eInvoice` (ohne den Gesellschaftscode), findet das System **keine passende Adresse** — die Rechnung wird intern blockiert und nicht zugestellt, ohne dass eine Fehlermeldung erscheint.

Diese Analyse durchleuchtet systematisch alle Kundenstammdaten und identifiziert, wo dieser Fehler vorliegt — und wie dringend eine Korrektur ist.

---

## Datenabruf

```{sql connection=cnn, output.var="df_zusammenfassung"}
SELECT
    c.DATAAREAID,
    COUNT(DISTINCT c.CUSTOMERACCOUNT)   AS Betroffene_Kunden,
    COUNT(*)                            AS Betroffene_Eintraege
FROM PartyContactsExport  d
    INNER JOIN CUSTCUSTOMERENTITY c
        ON  d.PARTYNUMBER  = c.PARTYNUMBER
    INNER JOIN CUSTTABLE ct
        ON  ct.ACCOUNTNUM  = c.CUSTOMERACCOUNT
        AND ct.DATAAREAID  = c.DATAAREAID
WHERE d.TYPE    = 2
  AND d.PURPOSE = 'eInvoice'
  AND ct.CUSTGROUP NOT LIKE '%IC'
GROUP BY c.DATAAREAID
ORDER BY c.DATAAREAID;
```


```{sql connection=cnn, output.var="df_kritikalitaet"}
SELECT
    c.CUSTOMERACCOUNT,
    c.DATAAREAID,
    d_falsch.LOCATOR    AS Email_Falsch,
    CASE
        WHEN d_korrekt.PURPOSE IS NULL
            THEN 'KRITISCH'
        ELSE 'Bereinigen'
    END                 AS Kritikalitaet
FROM CUSTCUSTOMERENTITY c
    INNER JOIN CUSTTABLE ct
        ON  ct.ACCOUNTNUM  = c.CUSTOMERACCOUNT
        AND ct.DATAAREAID  = c.DATAAREAID
    INNER JOIN PartyContactsExport d_falsch
        ON  d_falsch.PARTYNUMBER = c.PARTYNUMBER
        AND d_falsch.TYPE        = 2
        AND d_falsch.PURPOSE     = 'eInvoice'
    LEFT JOIN PartyContactsExport d_korrekt
        ON  d_korrekt.PARTYNUMBER = c.PARTYNUMBER
        AND d_korrekt.TYPE        = 2
        AND d_korrekt.PURPOSE     = CONCAT('eInvoice_', c.DATAAREAID)
WHERE ct.CUSTGROUP NOT LIKE '%IC'
ORDER BY Kritikalitaet DESC, c.DATAAREAID, c.CUSTOMERACCOUNT;
```

```{sql connection=cnn, output.var="df_luecken_regulaer"}
SELECT
    inv.ORDERACCOUNT    AS CUSTOMERACCOUNT,
    inv.DATAAREAID,
    ct.CUSTGROUP,
    COUNT(DISTINCT inv.INVOICEID)   AS Anzahl_Rechnungen,
    MAX(inv.INVOICEDATE)            AS Letzte_Rechnungsdatum
FROM CUSTINVOICEJOUR inv
    INNER JOIN CUSTTABLE ct
        ON  ct.ACCOUNTNUM  = inv.ORDERACCOUNT
        AND ct.DATAAREAID  = inv.DATAAREAID
    INNER JOIN CUSTCUSTOMERENTITY c
        ON  c.CUSTOMERACCOUNT = inv.ORDERACCOUNT
        AND c.DATAAREAID      = inv.DATAAREAID
    LEFT JOIN PartyContactsExport d_email
        ON  d_email.PARTYNUMBER = c.PARTYNUMBER
        AND d_email.TYPE        = 2
        AND d_email.PURPOSE     = CONCAT('eInvoice_', inv.DATAAREAID)
WHERE ct.CUSTGROUP NOT LIKE '%IC'
GROUP BY inv.ORDERACCOUNT, inv.DATAAREAID, ct.CUSTGROUP
HAVING MAX(d_email.PURPOSE) IS NULL
ORDER BY inv.DATAAREAID, inv.ORDERACCOUNT;
```

```{sql connection=cnn, output.var="df_luecken_aktiv_12m"}
SELECT
    inv.ORDERACCOUNT    AS CUSTOMERACCOUNT,
    inv.DATAAREAID,
    COUNT(DISTINCT inv.INVOICEID)   AS Anzahl_Rechnungen_12M,
    MAX(inv.INVOICEDATE)            AS Letzte_Rechnungsdatum
FROM CUSTINVOICEJOUR inv
    INNER JOIN CUSTTABLE ct
        ON  ct.ACCOUNTNUM  = inv.ORDERACCOUNT
        AND ct.DATAAREAID  = inv.DATAAREAID
    INNER JOIN CUSTCUSTOMERENTITY c
        ON  c.CUSTOMERACCOUNT = inv.ORDERACCOUNT
        AND c.DATAAREAID      = inv.DATAAREAID
    LEFT JOIN PartyContactsExport d_email
        ON  d_email.PARTYNUMBER = c.PARTYNUMBER
        AND d_email.TYPE        = 2
        AND d_email.PURPOSE     = CONCAT('eInvoice_', inv.DATAAREAID)
WHERE ct.CUSTGROUP NOT LIKE '%IC'
  AND inv.INVOICEDATE >= DATEADD(MONTH, -12, GETDATE())
GROUP BY inv.ORDERACCOUNT, inv.DATAAREAID, ct.CUSTGROUP
HAVING MAX(d_email.PURPOSE) IS NULL
ORDER BY Anzahl_Rechnungen_12M DESC, inv.DATAAREAID, inv.ORDERACCOUNT;
```

```{r kennzahlen, include=FALSE}
n_kunden_gesamt  <- sum(df_zusammenfassung$Betroffene_Kunden)
n_eintraege      <- sum(df_zusammenfassung$Betroffene_Eintraege)
n_kritisch       <- sum(df_kritikalitaet$Kritikalitaet == "KRITISCH")
n_bereinigen     <- sum(df_kritikalitaet$Kritikalitaet == "Bereinigen")
n_luecken        <- nrow(df_luecken_regulaer)
n_luecken_aktiv  <- nrow(df_luecken_aktiv_12m)
n_gesellschaften <- nrow(df_zusammenfassung)
```

---

## Kennzahlen auf einen Blick

Die folgende Tabelle gibt einen ersten Überblick über das Ausmaß des Problems. Sie zeigt, wie viele Kunden betroffen sind und in welche Gruppen sie sich aufteilen. Diese Zahlen bilden die Grundlage für alle weiteren Abschnitte und die priorisierten Handlungsempfehlungen am Ende des Berichts.

```{r kennzahlen-tabelle}
knitr::kable(
  data.frame(
    Kennzahl = c(
      "Betroffene Gesellschaften",
      "Betroffene Kunden (falscher Purpose)",
      "Davon: KRITISCH — kein korrekter Purpose vorhanden",
      "Davon: Bereinigen — korrekter Purpose existiert parallel",
      "Kunden mit Rechnungen aber ohne jeglichen Purpose (Lücken)",
      "Davon: aktiv in den letzten 12 Monaten"
    ),
    Wert = c(
      n_gesellschaften,
      n_kunden_gesamt,
      n_kritisch,
      n_bereinigen,
      n_luecken,
      n_luecken_aktiv
    )
  ),
  col.names = c("Kennzahl", "Anzahl"),
  align = c("l", "r")
)
```

**Wie liest man diese Tabelle?**

- **Betroffene Gesellschaften:** Gibt an, in wie vielen der sieben rechtlichen Einheiten (0100, 0360, 0500, 0600, 0700, 0800, 0900) das Problem aufgetreten ist. Wenn alle Gesellschaften betroffen sind, ist das Problem systemweit — eine Bereinigung an einer Stelle allein reicht nicht aus.

- **Betroffene Kunden (falscher Purpose):** Das sind Kunden, bei denen im System ein E-Mail-Eintrag mit dem Zweck `eInvoice` (falsch) existiert. Diese Kunden glauben möglicherweise, Rechnungen per E-Mail zu erhalten — tun es aber nicht. Jede Zahl hier entspricht einem realen Kunden, dessen Rechnungen still blockiert werden.

- **KRITISCH:** Diese Kunden haben ausschließlich den falschen Eintrag (`eInvoice`), aber keinen richtigen. Das bedeutet: Die Rechnung wird mit Sicherheit **nicht** per E-Mail zugestellt. Hier besteht der dringendste Handlungsbedarf, da keine automatische Zustellung stattfindet.

- **Bereinigen:** Bei diesen Kunden existiert neben dem fehlerhaften Eintrag auch ein korrekter Eintrag (`eInvoice_XXXX`). Die E-Mail-Zustellung funktioniert daher **bereits** über den korrekten Eintrag. Die Bereinigung dient der Datenhygiene — sie verhindert zukünftige Verwirrung und potenzielle Fehlerquellen.

- **Kunden ohne Purpose (Lücken):** Diese Kunden haben Rechnungen erhalten, aber noch nie einen E-Mail-Eintrag mit dem Zweck `eInvoice_XXXX` gepflegt bekommen. Sie erhalten keine Rechnungen per E-Mail — nicht weil der Eintrag falsch ist, sondern weil er ganz fehlt. Dieser Personenkreis ist von den oben genannten Gruppen getrennt und wird im Abschnitt *Lückenanalyse* detailliert behandelt.

- **Aktiv in den letzten 12 Monaten:** Aus der Gruppe der Lücken-Kunden hat diese Untergruppe tatsächlich Rechnungen in den letzten zwölf Monaten erhalten. Diese Kunden sind besonders dringend — sie sind aktiv und erhalten aktuell keine E-Mail-Zustellung ihrer Rechnungen.

---

## Betroffene Kunden je Gesellschaft

Diese Tabelle schlüsselt die betroffenen Kunden nach Gesellschaft auf. Jede Zeile entspricht einer rechtlichen Einheit. Die Spalte „Betroffene Kunden" nennt die Anzahl der Kunden, bei denen der falsch konfigurierte Eintrag (`eInvoice` statt `eInvoice_XXXX`) gefunden wurde. Die Spalte „Betroffene Einträge" kann höher liegen, da ein Kunde unter Umständen in mehreren Gesellschaften geführt wird — ein Kunde, der sowohl in 0700 als auch in 0800 aktiv ist, erscheint in beiden Zeilen.

```{r zusammenfassung-tabelle}
knitr::kable(
  df_zusammenfassung,
  col.names = c("Gesellschaft", "Betroffene Kunden", "Betroffene Einträge"),
  align = c("l", "r", "r")
)
```

**Interpretation:** Gesellschaften mit besonders vielen betroffenen Kunden sollten vorrangig bearbeitet werden. Wenn eine Gesellschaft in der Tabelle nicht erscheint, bedeutet das, dass dort kein Kunde mit dem fehlerhaften `eInvoice`-Eintrag gefunden wurde — entweder weil die Daten dort korrekt gepflegt sind oder weil die E-Mail-Zustellung für diese Gesellschaft anders konfiguriert ist.

---

## Klassifizierung nach Schweregrad

Nicht alle fehlerhaften Einträge sind gleich dringend. Die folgende Klassifizierung unterscheidet zwei Kategorien, die sich in ihrer Auswirkung auf den Rechnungsversand fundamental unterscheiden:

- **KRITISCH** bedeutet: Der Kunde hat *ausschließlich* den falschen Eintrag `eInvoice`. Es gibt keinen zweiten, richtigen Eintrag daneben. Folge: Das System findet bei der Suche nach `eInvoice_XXXX` **nichts** — die Rechnung wird nicht per E-Mail versandt. Der Kunde bekommt seine Rechnung nicht. Dieser Zustand erfordert sofortige Korrektur.

- **Bereinigen** bedeutet: Neben dem falschen Eintrag `eInvoice` existiert *auch* ein korrekter Eintrag `eInvoice_XXXX`. Das System findet den richtigen Eintrag — die Rechnung wird also tatsächlich zugestellt. Das Problem ist hier nicht die fehlgeschlagene Zustellung, sondern die Unordnung im Datenstamm. Der fehlerhafte Eintrag sollte entfernt werden, damit er nicht in Zukunft zu Problemen führt (z.B. beim Löschen des korrekten Eintrags aus Versehen).

Das Balkendiagramm und die darunter folgende Tabelle zeigen, wie sich diese zwei Gruppen auf die einzelnen Gesellschaften verteilen.

```{r kritikalitaet-chart, fig.width=6, fig.height=3}
df_kritikalitaet |>
  count(DATAAREAID, Kritikalitaet) |>
  ggplot(aes(x = DATAAREAID, y = n, fill = Kritikalitaet)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("KRITISCH" = "#C0392B", "Bereinigen" = "#E67E22")) +
  labs(x = "Gesellschaft", y = "Anzahl Kunden", fill = NULL,
       title = "Falsche eInvoice-Einträge nach Gesellschaft und Schweregrad") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())
```

```{r kritikalitaet-tabelle}
df_kritikalitaet |>
  count(Kritikalitaet, DATAAREAID) |>
  tidyr::pivot_wider(names_from = DATAAREAID, values_from = n, values_fill = 0) |>
  knitr::kable(caption = "Anzahl Kunden je Schweregrad und Gesellschaft")
```

**Wie liest man diese Tabelle?** Jede Spalte entspricht einer Gesellschaft, jede Zeile einer Schweregrad-Kategorie. Die Zahl in einer Zelle gibt an, wie viele Kunden dieser Gesellschaft in die jeweilige Kategorie fallen. Gesellschaften mit einem hohen KRITISCH-Anteil sind besonders dringend — dort erhalten aktuell viele Kunden keine Rechnungen per E-Mail. Ein Wert von 0 bedeutet, dass in dieser Gesellschaft kein betroffener Kunde in der jeweiligen Kategorie existiert.

### KRITISCH — Übersicht (Top 2 je Gesellschaft)

Kunden ohne **jeglichen** korrekten Purpose: sie erhalten garantiert keine Rechnung per E-Mail.
Die vollständige Liste wird als Excel-Datei exportiert (siehe Hinweis unten).

Jede Zeile in dieser Vorschau entspricht einem konkreten Kunden mit einer konkreten E-Mail-Adresse, der aktuell **keine** Rechnung per E-Mail erhält — trotz vorhandener E-Mail-Adresse im System. Für diesen Kunden muss der gespeicherte Eintrag von `eInvoice` in `eInvoice_XXXX` umbenannt werden (wobei `XXXX` für den jeweiligen Gesellschaftscode steht, z.B. `eInvoice_0700` für Gesellschaft 0700). Nach dieser Umbenennung erkennt das System den Eintrag korrekt und stellt die Rechnung automatisch per E-Mail zu.

```{r kritisch-export, include=FALSE}
df_kritisch_komplett <- df_kritikalitaet |>
  filter(Kritikalitaet == "KRITISCH") |>
  select(CUSTOMERACCOUNT, DATAAREAID, Email_Falsch) |>
  arrange(DATAAREAID, CUSTOMERACCOUNT)

export_path <- paste0(
  dirname(normalizePath(knitr::current_input())),
  "/KRITISCH_Kunden_", format(Sys.Date(), "%Y%m%d"), ".xlsx"
)

openxlsx::write.xlsx(
  df_kritisch_komplett,
  file      = export_path,
  sheetName = "KRITISCH",
  overwrite = TRUE
)
```

```{r kritisch-preview}
df_kritikalitaet |>
  filter(Kritikalitaet == "KRITISCH") |>
  select(CUSTOMERACCOUNT, DATAAREAID, Email_Falsch) |>
  arrange(DATAAREAID, CUSTOMERACCOUNT) |>
  group_by(DATAAREAID) |>
  slice_head(n = 2) |>
  ungroup() |>
  knitr::kable(
    col.names = c("Kundennummer", "Gesellschaft", "E-Mail-Adresse"),
    caption   = paste0(
      "Vorschau: 2 Einträge je Gesellschaft — ",
      n_kritisch, " Kunden gesamt. ",
      "Vollständige Liste: KRITISCH_Kunden_", format(Sys.Date(), "%Y%m%d"), ".xlsx"
    )
  )
```

> **Hinweis zur Excel-Datei:** Die vollständige Liste aller KRITISCH-Kunden wurde parallel als Excel-Datei exportiert (`KRITISCH_Kunden_JJJJMMTT.xlsx`). Diese Datei enthält alle betroffenen Kundennummern, Gesellschaften und E-Mail-Adressen und sollte als Grundlage für die Korrekturarbeiten verwendet werden. Sie kann z.B. dem zuständigen Systemadministrator oder dem Key User übergeben werden, der die Änderungen in D365FO durchführt.

---

## Lückenanalyse: Kunden ohne Purpose

Diese Gruppe unterscheidet sich von den KRITISCH- und Bereinigen-Kunden grundlegend: Während die vorherigen Gruppen einen *falschen* Eintrag haben, haben diese Kunden **überhaupt keinen** Eintrag für den E-Mail-Rechnungsversand. Sie sind im System vorhanden, haben in der Vergangenheit Rechnungen erhalten — aber nie per E-Mail, weil der entsprechende Verwendungszweck schlicht nicht angelegt wurde.

Diese Kunden wurden bei der ursprünglichen Einrichtung des E-Mail-Rechnungsversands vermutlich vergessen oder waren zu diesem Zeitpunkt noch nicht als E-Mail-Kunden vorgesehen. Seitdem erhalten sie ihre Rechnungen auf anderem Weg (z.B. per Post oder manuell) — oder gar nicht, ohne dass jemand dies bemerkt hat.

Die folgende Tabelle zeigt, in welchen Gesellschaften wie viele solcher Kunden existieren, wie viele Rechnungen sie insgesamt erhalten haben und wann die letzte Rechnung gestellt wurde. Diese letzte Information ist besonders wichtig: Kunden, bei denen das letzte Rechnungsdatum aktuell ist, sind *aktiv* — bei ihnen besteht ein unmittelbares Handlungserfordernis.

```{r luecken-summary}
knitr::kable(
  df_luecken_regulaer |>
    group_by(DATAAREAID) |>
    summarise(
      Kunden        = n_distinct(CUSTOMERACCOUNT),
      Rechnungen    = sum(Anzahl_Rechnungen),
      Letzte_Rechnung = max(Letzte_Rechnungsdatum)
    ) |>
    arrange(DATAAREAID),
  col.names = c("Gesellschaft", "Kunden ohne Purpose", "Rechnungen gesamt", "Letzte Rechnung"),
  caption = paste0("Gesamt: ", n_luecken, " Kunden — davon ", n_luecken_aktiv,
                   " aktiv in den letzten 12 Monaten")
)
```

**Wie liest man diese Tabelle?**

- **Gesellschaft:** Die rechtliche Einheit. Da Kunden gesellschaftsübergreifend geführt werden können, kann derselbe Kunde in mehreren Zeilen auftauchen.
- **Kunden ohne Purpose:** Anzahl der Kunden dieser Gesellschaft, die mindestens eine Rechnung erhalten haben, aber keinen `eInvoice_XXXX`-Eintrag besitzen.
- **Rechnungen gesamt:** Wie viele Rechnungen diese Kunden insgesamt erhalten haben — ein Hinweis auf das wirtschaftliche Gewicht dieser Gruppe. Gesellschaften mit sehr vielen Rechnungen haben entsprechend mehr Kundenkommunikation, die bisher nicht per E-Mail lief.
- **Letzte Rechnung:** Das Datum der zuletzt gestellten Rechnung an einen Kunden dieser Gruppe. Liegt das Datum nah an heute, handelt es sich um aktive Geschäftsbeziehungen mit unmittelbarem Handlungsbedarf. Liegt es weit in der Vergangenheit, handelt es sich möglicherweise um inaktive Kunden, die eine niedrigere Priorität haben können.

**Handlungsempfehlung:** Beginnen Sie mit den Gesellschaften, die sowohl viele Kunden als auch ein aktuelles Rechnungsdatum aufweisen. Die `r n_luecken_aktiv` aktiv rechnungsempfangenden Kunden sollten als erste Welle bearbeitet werden — für diese Kunden muss ein neuer Eintrag mit dem Zweck `eInvoice_XXXX` (passend zur jeweiligen Gesellschaft) angelegt werden.

---

## Empfohlene Maßnahmen

Die folgende Tabelle fasst für jede Kategorie zusammen, welche konkrete Maßnahme erforderlich ist und mit welcher Dringlichkeit sie umgesetzt werden sollte. Sie ist als Entscheidungsgrundlage und Auftrag an die umsetzenden Stellen gedacht.

```{r massnahmen-tabelle}
knitr::kable(
  data.frame(
    Kategorie   = c("KRITISCH", "Bereinigen", "Lücke"),
    Beschreibung = c(
      "Nur falscher eInvoice-Eintrag, kein korrekter vorhanden",
      "Falscher eInvoice-Eintrag neben korrektem eInvoice_XXXX",
      "Rechnungen vorhanden, aber kein Purpose hinterlegt"
    ),
    Anzahl = c(n_kritisch, n_bereinigen, n_luecken),
    Massnahme = c(
      "PURPOSE umbenennen: eInvoice → eInvoice_XXXX",
      "Falschen Eintrag löschen (korrekter bleibt erhalten)",
      "Neuen eInvoice_XXXX-Eintrag anlegen"
    ),
    Prioritaet = c("Hoch", "Mittel", "Hoch (aktive Kunden)")
  ),
  col.names = c("Kategorie", "Beschreibung", "Anzahl", "Maßnahme", "Priorität")
)
```

**Was bedeutet jede Maßnahme konkret?**

- **KRITISCH → PURPOSE umbenennen:** In D365FO ist für den betroffenen Kunden eine E-Mail-Adresse hinterlegt, der der Verwendungszweck `eInvoice` zugeordnet ist. Dieser muss auf `eInvoice_XXXX` geändert werden (z.B. `eInvoice_0800` für Gesellschaft 0800). Es handelt sich um eine direkte Datenpflege im Kundenstamm — kein neuer Datensatz wird angelegt, der vorhandene Eintrag wird lediglich umbenannt. Nach der Änderung wird die nächste Rechnung automatisch per E-Mail zugestellt. **Priorität: Hoch** — diese Kunden erhalten aktuell keine E-Mails.

- **Bereinigen → Falschen Eintrag löschen:** Dieser Kunde hat zwei E-Mail-Einträge: einen falschen (`eInvoice`) und einen korrekten (`eInvoice_XXXX`). Die Zustellung funktioniert bereits über den korrekten Eintrag. Der falsche Eintrag wird entfernt, um den Datenstamm sauber zu halten. **Priorität: Mittel** — es besteht kein akuter Ausfall, aber ein Bereinigungsbedarf.

- **Lücke → Neuen Eintrag anlegen:** Für diesen Kunden existiert noch gar kein E-Mail-Eintrag für den Rechnungsversand. Es muss ein neuer Eintrag mit dem Verwendungszweck `eInvoice_XXXX` und der zugehörigen E-Mail-Adresse des Kunden erstellt werden. **Priorität: Hoch für aktive Kunden** — Kunden, die in den letzten 12 Monaten Rechnungen erhalten haben, sollten zuerst bearbeitet werden.

> **Empfohlene Reihenfolge der Umsetzung:** (1) Alle KRITISCH-Kunden — vollständige Liste in Excel exportiert. (2) Aktive Lücken-Kunden (letzte Rechnung ≤ 12 Monate). (3) Bereinigen-Kunden. (4) Inaktive Lücken-Kunden.

---

## Offene Punkte

Bevor die Bereinigung vollständig abgeschlossen werden kann, ist eine formale Entscheidung zu einer Abgrenzungsfrage erforderlich.

> ⚠️ **Entscheidung ausstehend: Intercompany-Kunden**
>
> Sollen IC-Kunden (`CUSTGROUP LIKE '%IC'`) dauerhaft aus der Rechnungs-E-Mail-Analyse ausgeschlossen werden?
>
> **Hintergrund:** Intercompany-Kunden (IC-Kunden) sind konzerninterne Geschäftspartner — also Tochter- oder Schwestergesellschaften, die untereinander Waren oder Leistungen in Rechnung stellen. Bei diesen Kunden läuft der Rechnungsaustausch typischerweise über einen automatisierten konzerninternen Prozess, nicht über den regulären E-Mail-Rechnungsversand. Ein fehlender oder falscher `eInvoice_XXXX`-Eintrag bei IC-Kunden ist daher möglicherweise kein Fehler, sondern beabsichtigt.
>
> In dieser Analyse wurden IC-Kunden aus technischen Gründen bereits vorsorglich ausgeschlossen — sie sind in keiner der obigen Zahlen enthalten. Sollte die Entscheidung lauten, dass IC-Kunden ebenfalls betroffen sind und E-Mail-Einträge benötigen, müsste die Analyse wiederholt und die Maßnahmen entsprechend ausgeweitet werden.
>
> | | |
> |-|-|
> | **Entschieden von:** | ___________________________ |
> | **Datum:** | ___________________________ |
> | **Status:** | ☐ offen &emsp; ☐ bestätigt &emsp; ☐ abgelehnt |

---

*Erstellt mit R/RMarkdown — Detailanalyse: `D365FO_Email_Verwendungszweck_Analyse.Rmd`*
