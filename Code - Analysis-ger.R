# Packages und Vorbereitung ----
# Alle Analysen wurden mit R-Version 4.0.2 und RStudio 1.3.959 durchgeführt. Verwendete Packages werden hier gesammelt gelistet:

packages <- c("dplyr", "readr", "tidyr", "stringi", "tibble", "ggplot2", "reshape2", "corrplot", "cowplot", "lubridate", "magrittr", "tm", "stm", "RColorBrewer")
for (pkg in packages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
}
rm(packages, pkg)

{
  library(dplyr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(reshape2)
  library(RColorBrewer)
  library(corrplot)
  library(cowplot)
  library(tidyr)
  library(stringi)
  library(lubridate)
  library(quanteda)
  library(magrittr)
  library(tm)
  library(stm)
  
  set.seed(2020)
  setwd("Y:\Twitter Bachelor")
}

### Der hier präsentierte Code ist als konstant durchlaufendes Script gedacht - Vollständiges Markieren und Ausführen ist also möglich, wird aber aufgrund der voraussichtlichen Rechenzeit nicht angeraten. Da einige der im Folgenden erzeugen Dateien aus aufwändigen und/oder rechenintensiven Schritten entstehen, besteht die Möglichkeit, diese komplexeren Elemente direkt zu laden. Aus diesem Grund werden sich an einzelnen Punkten in auskommentierter Form die Codes zum Speichern und Laden von Workspace-Dateien der jeweils erzeugten Daten finden.
# Sollte man nach einer bestimmten Datei suchen, oder einen überblick über alle verfügbaren Workspace-Elemente haben wollen, so findet sich im zweiten R-Script ("Code - Data Loading.R") der Speicher- und Ladecode gebündelt und in übersichtlicher Form.

