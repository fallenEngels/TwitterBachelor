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
  setwd("Y:/Don/Twitter Bachelor")
}

# Verwendete Twitter-Datensätze ----
users <- read_csv("Twitter Data/ira_users_csv_hashed.csv") # Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)

tweets <- read_csv("Twitter Data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)

# test <- head(tweets, 20)

# Filtering und erste Analysen ----

replies <- tweets %>% filter(!(is.na(in_reply_to_userid)))
retweets <- tweets %>% filter(is_retweet ==T)


table(replies$in_reply_to_tweetid %in% tweets$tweetid)
32031 / 537884 * 100
table(retweets$retweet_tweetid %in% tweets$tweetid)
618157 / 3332424 * 100
# Etwas weniger als ein Viertel der Retweets kommen von innerhalb des Datensets, während Antworten zu beinahe 95% an Accounts außerhalb des Datensets gerichtet werden.
rm(retweets)
length(unique(replies$in_reply_to_tweetid[replies$in_reply_to_tweetid %in% tweets$tweetid]))
length(!(is.na(replies$in_reply_to_tweetid[replies$in_reply_to_tweetid %in% tweets$tweetid])))
length(unique(replies$in_reply_to_userid[replies$in_reply_to_tweetid %in% tweets$tweetid]))
sum(tweets$reply_count[tweets$tweetid %in% replies$in_reply_to_tweetid])
max(tweets$reply_count[tweets$tweetid %in% replies$in_reply_to_tweetid])
summary(tweets$reply_count[tweets$tweetid %in% replies$in_reply_to_tweetid])
# 18.000 einzigartige Tweets von 674 Accounts innerhalb des Datensets erhielten 32.031 Antworten von den hier vorhandenen Accounts - bzw. 30% ihrer Gesamt-Antworten, und das obwohl der reichweitenstärkste Tweet 3249 Antworten wurde.

# Reply - Grobe Analysen ----
rpl_df <- data.frame(from = as.character(), to = as.character(), occurances = as.integer())
# Edge-List für igraph-Plots, kann dann weiter gefiltert werden. "occurance" als Gewicht

for(i in 1:nrow(replies)) {
  user <- replies$userid[i]
  rep <- replies$in_reply_to_userid[i]
  if(user %in% rpl_df[, 1] & rep %in% rpl_df[, 2]) {
    row <- which(rpl_df[, 1] == user & rpl_df[, 2] == rep)
    rpl_df[row, 3] <- as.integer(rpl_df[row, 3]) + 1
  } else {
    rpl_df <- rbind(rpl_df, c(user, rep, 1))
  }
}
names(rpl_df) <- c("from", "to", "occurances")
rpl_df$occurances <- as.integer(rpl_df$occurances)

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

rpl_src <- data.frame(user = users$userid, replies = 0, followers = users$follower_count, tweets = 0)
for(i in 1:nrow(rpl_src)) {
  df <- tweets %>% filter(userid == rpl_src$user[i])
  rpl_src$tweets[i] <- nrow(df)
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
rm(occ, rep, row, user, test, df)

# Grafik Quellen-Accounts (für Netz #1)
ggplot(rpl_src[rpl_src$replies>0, ], aes(x = followers, y = replies, color = tweets)) + 
  geom_point() + theme_minimal() + 
  scale_color_binned(name = "Tweet-Zahl", low = "red", high = "green", trans = "sqrt",
                       breaks = c(1000, 5000, 10000, 25000, 50000, 100000), labels = scales::comma) +
  scale_x_sqrt(labels = scales::comma, breaks = c(1000, 10000, 25000, 50000, 100000, 200000)) + 
  scale_y_sqrt(labels = scales::comma, breaks = c(100, 1000, 2500, 5000, 10000, 20000, 30000))  +
  labs(x = "Follower-Zahl", y = "Anzahl an verfassten Antworten", title = "Follower je Account",
       subtitle = "Nach Anzahl verfasster Antworten und Tweets")
# Ohne Ausreißer
ggplot(rpl_src[rpl_src$replies>0 & rpl_src$replies<30000, ], aes(x = followers, y = replies, color = tweets)) + 
  geom_point() + theme_minimal() + 
  scale_color_binned(name = "Tweet-Zahl", low = "red", high = "green", trans = "sqrt",
                     breaks = c(1000, 5000, 10000, 25000, 50000, 100000), labels = scales::comma) +
  scale_x_sqrt(labels = scales::comma, breaks = c(1000, 10000, 25000, 50000, 100000, 200000)) + 
  scale_y_sqrt(labels = scales::comma, breaks = c(100, 1000, 2500, 5000, 10000, 20000, 30000))  +
  labs(x = "Follower-Zahl", y = "Anzahl an Antworten", title = "Follower je Account nach Antwort- und Tweet-Anzahl")

# Wer antwortet - Der Spam-Account ----

table(rpl_src$replies)
table(rpl_dst$occurance)
plot(as.integer(names(table(rpl_dst$occurance))))
plot(as.integer(names(table(rpl_src$replies))))
66898 / 82806 * 100
# Deutliche Unterschiede in der Anzahl an Interaktionen mit Accounts. Während sich bei den Ziel-Accounts, auf die geantwortet wurde, ein relativ gleichförmiger Anstieg in der Antwortzahl erkennen lässt, dominiert bei den Antwort-Accounts ein einzelner Account mit 30k Antworten - mehr als das 7fache des Accounts mit den zweitmeisten Antworten

g <- graph.data.frame(rpl_df)
degcent.g_in <- degree(g, normalized = T, mode = "in")
degcent.g_out <- degree(g, normalized = T, mode = "out")

head(sort(degcent.g_in, decreasing = T), 20)
head(sort(degcent.g_out, decreasing = T), 20)
# Während sich bei den in-degree-Accounts kein deutlicher Unterschied in der Zentralität zeigt, so dominiert bei den out-degrees der bereits im vorherigen Schritt als bedeutend identifizierte Account, mit einem fast zehnmal so hohen Wert wie Platz 2.

max(rpl_src$replies)
table(rpl_df$occurances[rpl_df$from == rpl_src$user[rpl_src$replies == max(rpl_src$replies)]])
#Dieser dominierende Account setzt dabei hauptsächlich auf Antworten an viele unterschiedliche Accounts, nur verhältnismäßig wenige finden sich mehrfach in den Daten.
gdom.answered <- rpl_df %>% filter(from == rpl_src$user[rpl_src$replies == max(rpl_src$replies)])
table(gdom.answered$to %in% users$userid)
# Keiner der Accounts, denen dieser Account geantwortet hat, finden sich in diesem Datensatz
for.gdom <- rpl_df %>% filter(from == rpl_src$user[rpl_src$replies == max(rpl_src$replies)]) %>%
  filter(occurances >= 6)
g.dom <- graph.data.frame(for.gdom)
V(g.dom)$color <- ifelse(V(g.dom)$name %in% rpl_src$user, "orange", "light blue")
plot.igraph(g.dom, 
            edge.width=E(g.dom)$occurances /4)

# Aber was tweetet dieser Account?
dom_out.id <- users$userid[which(substr(users$userid, 1, 8) == 
                                   substr(names(head(sort(degcent.g_out, decreasing = T), 1)), 1, 8))]
dom_out.twe <- tweets %>% filter(userid == dom_out.id)

head(dom_out.twe$tweet_text)
min(dom_out.twe$tweet_time); max(dom_out.twe$tweet_time)
table(!(is.na(dom_out.twe$in_reply_to_tweetid)))
table(grepl("workout|exercise|weight", tolower(dom_out.twe$tweet_text)))
table(grepl("//t.co/", dom_out.twe$tweet_text))
head(dom_out.twe$tweet_text[!(grepl("workout|exercise|weight", tolower(dom_out.twe$tweet_text)))],20)
table(grepl("@", dom_out.twe$tweet_text))
# Der dominante Account hat in anderthalb Monaten Aktivität knapp 65.000 Tweets abgesetzt, taggt in quasi jedem Tweet andere Nutzer, gut die Hälfte seiner Tweets sind Antworten auf andere Nutzer und 41% seiner Tweets beinhalten die Worte "workout" bzw. "exercise". Es handelt sich hier also mit sehr großer Wahrscheinlichkeit um einen Spam-Bot.
rm(dom_out.twe, dom_out.id, g, degcent.g_in, degcent.g_out, for.gdom, g.dom, gdom.answered)


# Wer antwortet - Andere Viel-Antworter ----

for.gmany <- rpl_df %>% filter(from %in% rpl_src$user[rpl_src$replies >= 450 & rpl_src$replies <= 5000])
# 25 Accounts mit den meisten Antworten nach Ausschluss des gerade besprochenen Accounts
table(for.gmany$to %in% rpl_src$user)
32727 / (32727+78) *100
g.many <- graph.data.frame(for.gmany)
V(g.many)$color <- ifelse(V(g.many)$name %in% rpl_src$user, "orange", "light blue")
plot.igraph(g.many, vertex.size = 1.5, vertex.label = NA,
            edge.arrow.size = 0.025, 
            edge.width=sqrt(E(g.many)$occurances) / 2)
for.gmany[for.gmany$to %in% for.gmany$from & for.gmany$to != for.gmany$from, ]
# Abgesehen von unterschiedlichen Anzahlen an Selbst-Antworten zwischen den einzelnen Accounts zeigt sich für jeden der 25 Accounts dasselbe Bild: Ein beinahe vollständig abgeschottetes Cluster an Antworten, das dominant (~99,8%) Accounts außerhalb des Datensets antwortet. 
many.names <- filter(rpl_df, from %in% rpl_src$user[rpl_src$replies >= 450 & rpl_src$replies <= 5000])
many.names <- unique(many.names$from)
users$follower_count[users$userid %in% many.names]
rm(for.gmany, g.many)

# Wem wird geantwortet ----

# Maximal beantwortete Accounts
table(rpl_dst$occurance)
table(rpl_dst$user[rpl_dst$occurance == 1] %in% rpl_src$user)
# Dominant viele Accounts mit nur einer Antwort (verm. Spam an Accounts außerhalb der Daten, da zu 98,5% Ziel-Accounts nicht im Datensatz), teilweise Accounts mit mehreren Tausend Antworten, aber keine deutlich abgegrenzten Spitzen -> Analyse der top beantworteten Accounts für mehr Klarheit: Cutoff 500 Antworten, 38 Accounts)
dat <- data.frame(occur = as.vector(table(rpl_dst$occurance)), 
                  interact = as.integer(names(table(rpl_dst$occurance))), 
                  out.user = 0, in.user = 0)
for(i in 1:nrow(dat)){
  ft <- table(rpl_dst$user[rpl_dst$occurance == dat$interact[i]] %in% rpl_src$user)
  names(ft)
  if(length(ft) == 2){
    dat[i, 3:4] <- ft
  } else {
    if(names(ft) == "FALSE"){
      dat[i, 3] <- ft
    } else {
      dat[i, 4] <- ft
    }
  }
}
dat$out_perc <- dat$out.user / (dat$out.user + dat$in.user) * 100
ggplot(dat, aes(x = interact, y = out_perc)) + geom_smooth(method = "loess") + 
  geom_point(aes(color = occur), size = 2) + 
  scale_color_binned(name = "Anzahl an\nAccounts", trans = "log", 
                     breaks = c(2, 10, 100, 1000, 10000), low = "red", high = "black") +
  coord_cartesian(ylim = c(0,100)) + scale_x_log10() + theme_minimal() +
  labs(title = "Verteilung der beantworteten Accounts nach Interaktionszahl",
       x = "Anzahl an Interaktionen",
       y = "Prozentsatz an Accounts nicht im Datensatz")
# Accounts mit unter ~ 60 Antworten finden sich deutlich öfter nicht im Datensatz, Accounts mit >1000 Antworten finden sich deutlich öfter innerhalb des Datensatzes.

for.gmaxrep <- rpl_df %>% filter(to %in% rpl_dst$user[rpl_dst$occurance >= 200])
table(unique(for.gmaxrep$to) %in% rpl_df$from)

g.maxrep <- graph.data.frame(for.gmaxrep, directed = T)
g.maxrep
V(g.maxrep)$color <- ifelse(V(g.maxrep)$name %in% rpl_src$user, "red", "light blue")
V(g.maxrep)$color <- ifelse(grepl("hash$", V(g.maxrep)$name), "orange", V(g.maxrep)$color)
l <- layout_with_fr(g.maxrep)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot.igraph(g.maxrep, layout = l * 1000, vertex.size = 2, vertex.label.cex = 0.5,
            edge.width=sqrt(E(g.maxrep)$occurances)/4, edge.arrow.size = 0.15)
# Empfehlung: Export als pdf, 25' x 25' für beste Lesbarkeit

# Obwohl 95% der Antworten an Accounts außerhalb des Netzwerkes gerichtet sind, finden sich in den Accounts mit den meisten Antworten deutliche Muster:
#   -> Nur 11 der 38 Accounts mit den meisten Replys stammen von außerhalb des Datensatzes
#   -> Die Accounts außerhalb des Netzwerkes, die hier auftauchen, sind mit je ein bis drei Netzwerk-Accounts verbunden, die ihnen antworten
#   -> mehrere dezentrale Netzwerke finden sich in den Daten, die sich relativ gleichmäßig gegenseitig antworten
#   -> drei Accounts, der sich dominant bzw. ausschließlich selbst antwortet. Einer ist gleichzeitig Zentrum eines der
#   -> sechs Cluster, in denen sich Accounts um einen zentralen Account scharen und diesem Antworten. Alle diese zentralen Accounts befinden sich in den Daten, und die Netzwerkgrößen schwanken deutlich (6 bis >100 Antwort-Accounts.)
#   -> zwei der größten Cluster sind direkt über eine vergleichsweise kleine Interaktion miteinander verbunden, während zwei mittlere und ein kleiner durch eine Antwort-"Achse" miteinander verbunden sind, die die stärksten Interaktionen zwischen zwei Accounts aufweist und mehrere der dezentralen Cluster mit einschließt.

# Accounts in den Zentren der Cluster:
degcent.gmaxr_in <- degree(g.maxrep, normalized = T, mode = "in")
head(sort(degcent.gmaxr_in, decreasing = T), 20)
clust <- head(names(sort(degcent.gmaxr_in, decreasing = T)), 12)
clust <- clust[clust %in% users$userid]
clust
clust.users <- users %>% filter(users$userid %in% clust)
clust.users$account_creation_date
clust.users$follower_count
clust.users$user_profile_description
clust.en.tweets1 <- tweets %>% filter(userid == "2484642526")
clust.en.tweets2 <- tweets %>% filter(userid == "2912754262")
clust.en.tweets3 <- tweets %>% filter(userid == "2944766250")
head(filter(tweets, in_reply_to_userid == "2484642526" & userid != "2484642526")$tweet_text, 50)
head(filter(tweets, in_reply_to_userid == "2912754262" & userid != "2912754262")$tweet_text, 50)
head(filter(tweets, in_reply_to_userid == "2944766250" & userid != "2944766250")$tweet_text, 50)
clust.en.tweets$tweet_text[sample(1:nrow(clust.en.tweets), 20)]
# Durchaus Tweets mit politischen Orientierungen, denen hier geantwortet wird.
test <- filter(rpl_df, to == "2484642526")
# rm(for.gmaxrep, g.maxrep, degcent.gmaxr_in, degcent.gmaxr_out, my_breaks)
