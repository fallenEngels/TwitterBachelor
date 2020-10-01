packages <- c()
for (pkg in packages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
}
rm(packages, pkg)

{
  library(dplyr)
  library(readr)
  
  set.seed(2020)
  setwd("E:\\Twitter Bachelor")
}

# Verwendete Twitter-Datensätze ----
users <- read_csv("Twitter Data/ira_users_csv_hashed.csv") # Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)

tweets <- read_csv("Twitter Data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)

test <- head(tweets, 20)

replys <- tweets %>% filter(!(is.na(in_reply_to_userid)))

retweets <- tweets %>% filter(is_retweet == T)

table(replys$in_reply_to_tweetid %in% tweets$tweetid)
table(retweets$retweet_tweetid %in% tweets$tweetid)
