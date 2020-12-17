
### Alle in dieser Datei aufgeführten Schritte sind als organisierter Zusatz zum Haupt-Script ("Code - Analysis.R") gedacht. Den hier präsentierten Code in seiner Gesamtheit auszuführen, führt zu keinerlei logischen und zielführenden Ergebnissen. Die folgenden Ladebefehle dienen rein der besseren Übersicht, falls eine bestimmte Datei im Hauptscript benötigt wird, die derzeit nicht geladen ist.

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
  library(stringr)
  library(lubridate)
  library(quanteda)
  library(magrittr)
  library(tm)
  library(stm)
  
  set.seed(2020)
  setwd("Y:/Twitter Bachelor")
}

# Verwendete Twitter-Datensätze ----

users <- read_csv("Twitter Data/ira_users_csv_hashed.csv") # Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)

tweets <- read_csv("Twitter Data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)
tweets_oldver <- read_csv("Twitter Data/ira_tweets_csv_hashed_alt.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_character(), longitude = col_character(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 15.10.2018

# Analyse Tweets ----
load("Saved Files/users_ggplot.RData")

# Cleanup Data Sets ----
tweets_eng <- read_csv("Twitter Data/tweets_en-cleaned.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))

tweets_clean <- tweets_eng[which(!(duplicated(tweets_eng$tweet_text))), ]
rm(tweets_eng)

# STM - Vorbereitung und Suche nach K ----
load("Saved Files/stm_dtm.RData")

used_documents <- names(stm_dtm$documents)
used_documents <- used_documents %>% gsub("^text", "", .) %>% as.integer(.)

load("selectK.RData")

# STM - Interpretation ----
label_csv <- read.csv("Other Files/STM_TopicLabels.csv", sep = ';', stringsAsFactors = F)

load("Saved Files/stm_mod_90.RData")

tweets_stm <- read_csv("Other Files/tweets_stm.csv", col_types = cols(tweetid = col_character(), in_reply_to_tweetid = col_character()))
