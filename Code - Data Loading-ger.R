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

### Alle in dieser Datei aufgeführten Schritte sind als organisierter Zusatz zum Haupt-Script ("Code - Analysis.R") gedacht. Den hier präsentierten Code in seiner Gesamtheit auszuführen, führt zu keinerlei logischen und zielführenden Ergebnissen. Die folgenden Ladebefehle dienen rein der besseren Übersicht, falls eine bestimmte Datei im Hauptscript benötigt wird, die derzeit nicht geladen ist.

# Verwendete Twitter-Datensätze ----

users <- read_csv("Twitter Data/ira_users_csv_hashed.csv") # Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)
users_oldver <- read_csv("Twitter Data/ira_users_csv_hashed_alt.csv") # Datensatz der Nutzer, Version vom 15.10.2018

tweets <- read_csv("Twitter Data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)
tweets_oldver <- read_csv("Twitter Data/ira_tweets_csv_hashed_alt.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_character(), longitude = col_character(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 15.10.2018

