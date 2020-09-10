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