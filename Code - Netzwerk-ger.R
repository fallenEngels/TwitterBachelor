packages <- c("dplyr", "readr", "igraph", "ggplot2")
for (pkg in packages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
}
rm(packages, pkg)

{
  library(dplyr)
  library(readr)
  library(igraph)
  library(ggplot2)
  
  set.seed(2020)
  setwd("E:\\Twitter Bachelor")
}

# Verwendete Twitter-Datensätze ----
users <- read_csv("Twitter Data/ira_users_csv_hashed.csv") # Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)

tweets <- read_csv("Twitter Data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)

# test <- head(tweets, 20)

# Filtering und erste Analysen ----

replies <- tweets %>% filter(!(is.na(in_reply_to_userid)))
rm(tweets)
retweets <- tweets %>% filter(is_retweet == T)

table(replies$in_reply_to_tweetid %in% tweets$tweetid)
32031 / 505853 * 100
table(retweets$retweet_tweetid %in% tweets$tweetid)
618157 / 2714267 * 100
# Etwas weniger als ein Viertel der Retweets kommen von innerhalb des Datensets, während Antworten zu beinahe 95% an Accounts außerhalb des Datensets gerichtet werden.
length(unique(retweets$retweet_tweetid[retweets$retweet_tweetid %in% tweets$tweetid]))
length(unique(retweets$retweet_userid[retweets$retweet_tweetid %in% tweets$tweetid]))
sum(tweets$retweet_count[tweets$tweetid %in% retweets$retweet_tweetid])
summary(tweets$retweet_count[tweets$tweetid %in% retweets$retweet_tweetid])
# 184.000 einzigartige Tweets von 1734 innerhalb des Datensets erhielten 618.000 Retweets von den hier vorhandenen Accounts - bzw. 10% ihrer Gesamt-Retweets, und das obwohl der Reichweitenstärkste Tweet von 123.617 Accounts Retweetet wurde.

# Reply - Analysen ----
rpl_df <- data.frame(from = as.character(), to = as.character(), occurances = as.integer())

for(i in 1:nrow(replies)) {
  user <- replies$userid[i]
  rep <- replies$in_reply_to_userid[i]
  if(user %in% rpl_df[, 1] && rep %in% rpl_df[, 2]) {
    row <- which(rpl_df[, 1] == user & rpl_df[, 2] == rep)
    rpl_df[row, 3] <- as.integer(rpl_df[row, 3]) + 1
  } else {
    rpl_df <- rbind(rpl_df, c(user, rep, 1))
  }
}
names(rpl_df) <- c("from", "to", "occurances")

# Kürzung der Namen für bessere Lesbarkeit
test <- rpl_df # Kontrolle, ob durch Kürzung "falsche" Duplikate entstehen
for(i in 1:nrow(rpl_df)) {
  if(nchar(rpl_df$from[i]) == 64){
    rpl_df$from[i] <- paste0(substr(rpl_df$from[i], 1,8),"-hash")
  }
  if(nchar(rpl_df$to[i]) == 64){
    rpl_df$to[i] <- paste0(substr(rpl_df$to[i], 1,8),"-hash")
  }
}
table(duplicated(rpl_df$from) == duplicated(test$from))
table(duplicated(rpl_df$to) == duplicated(test$to))
# Alles gut, keine falschen Duplikate

# write_csv(rpl_df, file.path("Hausarbeit/reply_df.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")
# rpl_df <- read_csv("Hausarbeit/reply_df.csv")

rpl_src <- data.frame(user = users$userid, replies = 0, followers = users$follower_count)
for(i in 1:nrow(rpl_src)) {
  if(nchar(rpl_src$user[i]) == 64){
    rpl_src$user[i] <- paste0(substr(rpl_src$user[i], 1,8),"-hash")
  }
  if(rpl_src$user[i] %in% rpl_df$from){
    df <- rpl_df %>% filter(from == rpl_src$user[i])
    rpl_src$replies[i] <- nrow(df)
  }
}

rpl_dst <- data.frame(user = unique(rpl_df$to), occurance = 0, uniq.rep = 0, in.data = F)
for(i in 1:nrow(rpl_dst)) {
  occ <- rpl_df %>% filter(to == rpl_dst$user[i])
  rpl_dst$occurance[i] <- sum(occ$occurances)
  rpl_dst$uniq.rep[i] <- nrow(occ)
  rpl_dst$in.data[i] <- ifelse(rpl_dst$user[i] %in% rpl_src$user, T, F)
}

table(rpl_src$replies)
table(rpl_dst$occurance)
# Deutliche Unterschiede in der Anzahl an Interaktionen mit Accounts. Während sich bei den Ziel-Accounts, auf die geantwortet wurde, ein relativ gleichförmiger Anstieg in der Antwortzahl erkennen lässt, dominiert bei den Antwort-Accounts ein einzelner Account mit 30k Antworten - mehr als das 7fache des Accounts mit den zweitmeisten Antworten
table(rpl_df$occurances[rpl_df$from == rpl_src$user[rpl_src$replies == max(rpl_src$replies)]])
#Dieser dominierende Account setzt dabei hauptsächlich auf ANtworten an viele unterschiedliche Accounts, nur verhältnismäßig wenige finden sich mehrfach in den Daten.
for.gdom <- rpl_df %>% filter(from == rpl_src$user[rpl_src$replies == max(rpl_src$replies)]) %>%
  filter(occurances >= 6)
g.dom <- graph.data.frame(for.gdom)
V(g.dom)$color <- ifelse(V(g.dom)$name %in% rpl_src$user, "orange", "light blue")
plot.igraph(g.dom, 
            edge.width=E(g.dom)$occurances /4)
# Accounts, denen mehr als 5 mal geantwortet wurde, finden sich alle nicht in den Daten.

for.gmaxrep <- rpl_df %>% filter(to %in% rpl_dst$user[rpl_dst$occurance >= 500])

g.maxrep <- graph.data.frame(for.gmaxrep, directed = T)
g.maxrep
V(g.maxrep)$color <- ifelse(V(g.maxrep)$name %in% rpl_src$user, "orange", "light blue")
l <- layout_with_fr(g.maxrep)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot.igraph(g.maxrep, layout = l * 1000, vertex.size = 2, vertex.label.cex = 0.5,
            edge.width=sqrt(E(g.maxrep)$occurances)/2, edge.arrow.size = 0.25)

# Obwohl 95% der Antworten an Accounts außerhalb des Netzwerkes gerichtet sind, finden sich in den Accounts mit den meisten Antworten deutliche Muster:
#   -> Die 10 Accounts außerhalb des Netzwerkes, die hier auftauchen, sind mit je ein bis drei Netzwerk-Accounts verbunden, die ihnen antworten
#   -> mehrere dezentrale Netzwerke finden sich in den Daten, die sich relativ gleichmäßig gegenseitig antworten
#   -> drei Accounts, der sich dominant bzw. ausschließlich selbst antwortet. Einer ist gleichzeitig Zentrum eines der
#   -> sechs Cluster, in denen sich Accounts um einen zentralen Account scharen und diesem Antworten. Alle diese zentralen Accounts befinden sich in den Daten, und die Netzwerkgrößen schwanken deutlich (6 bis >100 Antwort-Accounts.)
#   -> zwei der größten Cluster sind direkt über eine vergleichsweise kleine Interaktion miteinander verbunden, während zwei mittlere und ein kleiner durch eine Antort-"Achse" niteinander verbunden sind, die die stärksten Interaktionen zwischen zwei Accounts aufweist und mehrere der dezentralen Cluster mit einschließt.



# Großteil der Verbindungen ziwschen Accounts nur einmalig und viele Accounts mit nur einer einzigen Verbindung, hier jedoch mehrfach auftretende Verbindungen interessant. Erste Erkenntnis - Maxima: 2011-faches Auftreten der selben Vrbindung zweier Accounts, ein einzelner Account mit 30000 replies in den Daten.
g.mid <- graph.data.frame(rpl_df[rpl_df$from %in% rpl_sum$users[rpl_sum$replies > 1], ], directed = T)
g.mid
deg.dist <- degree.distribution(g.mid)
deg.dist # Große Verteilung der einzelnen Gradmengen, weiteres Filtern notwendig.
g.mid <- graph.data.frame(rpl_df[rpl_df$from %in% rpl_sum$replies[rpl_sum$replies > 1 & rpl_sum$replies < 16], ]
                          , directed = T)
barplot(degree.distribution(g.mid))
# Betracht

# Accounts mit bis zu 15 degrees tauchen häufiger in den Daten auf. Jenseits der 15 degrees finden sich spradisch einzelne Accounts mitextremen Werten.
g.max <- delete.vertices(g, which(degree(g) < 15))
g.max
barplot(degree.distribution(g.max))
plot(g.max)
# 270 Accounts mit mehr als 15 Antworten finden sich in den Daten.
V(g.max)$color <- ifelse(grepl("hash$", V(g.max)$name), "orange", "blue")
plot(g.max,
     layout= layout_nicely(g.max), 
     vertex.size=degree(g.max) * 2,
     vertex.label.cex = 0.25,
     edge.width=E(g.max)$occurances / 100)




# Retweet - Analysen ----
rtw_df <- data.frame(from = retweets$userid, to = retweets$retweet_userid, 
                     retweetid = retweets$retweet_tweetid, tweetid = retweets$tweetid, 
                     rtfollowers = retweets$follower_count, tfollowers = numeric(length = nrow(retweets)), tretweets = numeric(length = nrow(retweets)))

rtw_log <- rtw_df$to %in% users$userid
for(i in 1:nrow(rtw_df)){
  if(rtw_log[i] ==T){
    rtw_df$tfollowers[i] <- users$follower_count[which(users$userid == rtw_df$to[i])]
  } else{
    rtw_df$tfollowers[i] <- NA
  }
}
rtw_src <- tweets %>% filter(tweetid %in% rtw_df$retweetid) %>% select(retweet_tweetid, retweet_count)
for(i in 1:100){
  if(rtw_log[i] ==T){
    rtw_df$treweets[i] <- rtw_src$retweet_count[which(rtw_src$tweetid == rtw_df$retweetid[i])]
  } else{
    rtw_df$treweets[i] <- NA
  }
}

write_csv(rtw_df, file.path("Hausarbeit/retweet_df.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")



