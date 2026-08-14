source("PFAD/ZU/EUREM/PROJEKT/appinsights_helpers.R")   # eure Datei mit get_appinsights_connection()/run_appinsights_query()
exists("run_appinsights_query", mode = "function")       # sollte TRUE sein

source("R/data_telemetry.R")
touches <- load_telemetry_touches(cfg, "kql", environment = "PRJ")
attr(touches, "status")   # "OK", "OK_EMPTY" oder "UNKNOWN"
head(touches)
