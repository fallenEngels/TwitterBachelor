
### Alle in dieser Datei aufgeführten Schritte sind als organisierter Zusatz zum Haupt-Script ("Code - Analysis.R") gedacht. Den hier präsentierten Code in seiner Gesamtheit auszuführen, führt zu keinerlei logischen und zielführenden Ergebnissen. Die folgenden Ladebefehle dienen rein der besseren Übersicht, falls eine bestimmte Datei im Hauptscript benötigt wird, die derzeit nicht geladen ist.

{
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(ggplot2)
  library(reshape2)
  library(corrplot)
  library(gridExtra)
  library(lubridate)
  library(quanteda)
  library(stm)
  
  setwd("Y:/Twitter Bachelor") # Y: mit jeweiligem Stick-Ordner ersetzen
}


# Verwendete Twitter-Datensätze ----

users <- as.data.frame(read_csv("Twitter Data/ira_users_csv_hashed.csv"))
# Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)

tweets <- as.data.frame(read_csv("Twitter Data/ira_tweets_csv_hashed.csv", 
                                 col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(),
                                                  in_reply_to_tweetid = col_character(), latitude = col_factor(), 
                                                  longitude = col_factor(), poll_choices = col_character()))) 
# Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)

# Deskriptive Analysen ----
tweets_eng <- tweets %>% filter(tweet_language == "en")
users_eng <- users %>% filter(userid %in% tweets_eng$userid)

# Cleanup Data Sets ----
tweets_eng <- read_csv("Twitter Data/tweets_en-norts-nodupes.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))

tweets_clean <- read_csv("Twitter Data/tweets_clean-nodupes.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))


# STM - Vorbereitung und Suche nach K ----
load("Saved Files/stm_dtm.RData")

load("Other Files/Documents.RData")
# Liste: used_documents

load("Saved Files/selectK.RData")


# STM - Interpretation ----
load("Saved Files/stm_mod_90.RData")

label_csv <- read.csv("Other Files/STM_TopicLabels.csv", sep = ';', stringsAsFactors = F)


# Rückbezug auf Originaltweets ----
tweets_stm <- read_csv("Other Files/tweets_stm.csv", col_types = cols(tweetid = col_character(), in_reply_to_tweetid = col_character(), quoted_tweet_tweetid = col_character()))


# Inhaltsanalysen ----
tweets_eng_raw <- read_csv("Twitter Data/tweets_en-norts.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))

load("Other Files/Spike_Users.RData")
# Liste: spike_usr
