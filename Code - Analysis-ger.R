# Packages und Vorbereitung ----
# Alle Analysen wurden mit R-Version 4.0.2 und RStudio 1.3.959 durchgeführt. Verwendete Packages werden hier gesammelt gelistet:

packages <- c("dplyr", "readr", "tidyr", "stringi", "stringr", "tibble", "ggplot2", "reshape2", "corrplot", "cowplot", "lubridate", "magrittr", "tm", "stm", "RColorBrewer", "scales")
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
  library(stringr)
  library(lubridate)
  library(quanteda)
  library(magrittr)
  library(tm)
  library(stm)
  
  set.seed(2020)
  setwd("Y:/Twitter Bachelor")
}

{
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(reshape2)
  library(ggplot2)
  library(corrplot)
  library(gridExtra)
  library(lubridate)
  library(quanteda)
  library(stm)
  
  setwd("Y:/Twitter Bachelor") # Y: mit jeweiligem Stick-Ordner ersetzen
}

### Der in dieser Datei präsentierte Code ist als konstant durchlaufendes Script gedacht - Vollständiges Markieren und Ausführen ist also möglich, wird aber aufgrund der voraussichtlichen Rechenzeit und der Dateigrößen nicht angeraten. Da einige der im Folgenden erzeugen Dateien aus aufwändigen und/oder rechenintensiven Schritten entstehen, besteht die Möglichkeit, diese komplexeren Elemente direkt zu laden. Aus diesem Grund werden sich an einzelnen Punkten in auskommentierter Form die Codes zum Speichern und Laden von Workspace-Dateien der jeweils erzeugten Daten finden.
# Sollte man nach einer bestimmten Datei suchen, oder einen überblick über alle verfügbaren Workspace-Elemente haben wollen, so findet sich im zweiten R-Script ("Code - Data Loading.R") der Speicher- und Ladecode gebündelt und in übersichtlicher Form.

# Sollte es zu Darstellungsfehlern bei Umlauten oder ähnlichen visuellen Problemen kommen, sind diese vermutlich durch ein Umstellen der Standard-Kodierung in den R-Globaloptionen auf UTF-8 und ein neues Öffnen des Scripts zu beheben.



# Verwendete Twitter-Datensätze ----

users <- as.data.frame(read_csv("Twitter Data/ira_users_csv_hashed.csv"))
# Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)
tweets <- as.data.frame(read_csv("Twitter Data/ira_tweets_csv_hashed.csv", 
                                 col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(),
                                                  in_reply_to_tweetid = col_character(), latitude = col_factor(), 
                                                  longitude = col_factor(), poll_choices = col_character()))) 
# Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)

# Nutzung von read_csv (readr) statt read.csv (base), da base-Funktion ohne großen Aufwand nicht zur Darstellung der unterschiedlichen Schriftsätze (westlich, kyrillisch, arabisch, ...) innerhalb einer Datei fähig zu sein scheint.

# !ACHTUNG!
# Es ist aufgrund der schieren Größe des Tweet-Datensatzes (>5GB) je nach vorhandenem Arbeitsspeicher dazu zu raten, diesen nur dann zu laden, wenn er aktiv benötigt wird und nach Gebrauch zu entladen ( rm(NAME) ). Sollte dies den Speicher nicht komplett leeren, kann mit einer Garbage Collection oder R-Neustart nachgeholfen werden ( gc() bzw. .rs.restartR() respektive).

# !ACHTUNG 2!
# Es kann scheinbar bei der Nutzung von dplyr vorkommen, dass scheinbar zufällige Befehle die Warnmeldungen "Unknown or uninitialized column" produzieren. Diese Warnmeldung sollte jedoch im Normalfall zu ignorieren sein und hat nichts mit dem ausgeführten Befehl zu tun: https://github.com/tidyverse/tibble/issues/450 




# Deskriptive Analysen: ----

### Account-Sprachen
acc_lang <- data.frame(sort(table(users$account_language), decreasing = T))
acc_lang$perc <- acc_lang$Freq / nrow(users) *100
acc_lang
# Vermutlich ISO 639-1 bzw. lokalisierte (en-gb, zh-cn) Tags. Der Großteil der Accounts sind englischsprachig eingestellt, aber ein knappes Drittel gibt russisch als Sprache an. West- u. Zentraleuropäische Accounts sind die drittgrößte Gruppe (Deutschland, UK, Frankreich, Spanien, Italien) vor arabischen Accounts. Vereinzelte Chinesen, Indonesier und Ukrainer.
twt_lang <- data.frame(sort(table(tweets$tweet_language), decreasing = T))
twt_lang$perc <- twt_lang$Freq / nrow(tweets) * 100
twt_lang$perc <- format(twt_lang$perc, scientific = F, digits = 1)
twt_lang
tweets$tweet_text[tweets$tweet_language %in% c("ug", "ps", "iu")]
# Obwohl die meisten Accounts englischsprachig eingestellt sind, sind dominant mehr Tweets in russischer Sprache verfasst, mit englischen Tweets auf Platz 2 - Tweeten englisch eingestellte Accounts auf russisch oder tweeten russisch eingestellte Accounts mehr als englische?
table(tweets$account_language == tweets$tweet_language) / nrow(tweets) * 100
# In 72,6% der Fälle stimmt die Tweet-Sprache mit der gewählten Account-Sprache überein. Russischsprachige Accounts scheinen also einfach mehr getweetet zu haben als englischsprachige


# Exklusivität Englisch und Russisch - Grafische Veranschaulichung
lang_excl <- data.frame(userid = users$userid, tweets = 0, eng = 0, rus = 0, oth = 0, eng_perc = 0, rus_perc = 0, oth_perc = 0)
for(i in 1:nrow(lang_excl)){
  usr_twt <- tweets %>% filter(userid == lang_excl$userid[i]) %>% select(userid, tweet_language) %>% 
    filter(!(is.na(tweet_language)))
  lang_excl$tweets[i] <- nrow(usr_twt)
  lang_excl$eng[i] <- nrow(usr_twt[usr_twt$tweet_language == "en", ])
  lang_excl$rus[i] <- nrow(usr_twt[usr_twt$tweet_language == "ru", ])
  lang_excl$oth[i] <- lang_excl$tweets[i] - (lang_excl$eng[i] + lang_excl$rus[i])
  lang_excl$eng_perc[i] <- lang_excl$eng[i] / lang_excl$tweets[i] * 100
  lang_excl$rus_perc[i] <- lang_excl$rus[i] / lang_excl$tweets[i] * 100
  lang_excl$oth_perc[i] <- lang_excl$oth[i] / lang_excl$tweets[i] * 100
} # This might take a while
rm(usr_twt, i)
lang_excl.long <- lang_excl %>% select(userid, eng_perc, rus_perc, oth_perc) %>% filter(!(is.nan(eng_perc)))
lang_excl.long <- melt(lang_excl.long, id.vars = "userid")

lang_excl.long %>% ggplot(aes(x = variable, y = value, fill = variable)) + geom_violin() + theme_minimal() +
  labs(x = "Sprache", y = "Anteil an Tweets je Account in %") + theme(legend.position="none") +
  scale_x_discrete(labels=c("Englisch", "Russisch", "Andere")) +
  scale_fill_discrete(name = "Sprache", labels = c("Englisch", "Russisch", "Andere"))
# Sprachauswahl scheint dominant Accounts zu bestimmen, kaum Accounts unter ~80% einer Sprache -> gute Filtermöglichkeit, Dominanz anderer Sprachen on ~5-10% je Account lässt strukturelles Mislabeling bestimmter (zu kurzer?) Tweets vermuten
rm(lang_excl.long)
# "Falsche" Sprachen genauer analysiert
table(tweets$account_language != tweets$tweet_language)[2] # 2,1 Mio "falsch" gelabelte Tweets
data.frame(sort(table(tweets$account_language[tweets$account_language != tweets$tweet_language]), decreasing = T))[1:5,]
data.frame(sort(table(tweets$tweet_language[tweets$account_language != tweets$tweet_language]), decreasing = T))[1:5,]
# Es scheint sich bei den Unterschieden zwischen Account- und Tweet-Sprache dominant um russischsprachige Accounts mit englischer Spracheinstellung zu handeln, da 1,7 Mio der 2,1 Mio "Fehler" englische Account-Sprache, und 1,4 Mio russische Tweet-Sprache angeben
rm(twt_lang, acc_lang)



### Sprachfilter: Da es hier um die Beeinflussung der USA gehen soll, sind nur englischsprachige Tweets von Interesse - Der Anteil von Amerikanern, die russisch, ukrainisch oder eine der anderen Sprachen beherrscht und auf zufällig aufauchende Tweets in diesen Sprachen reagiert sollte nicht ausreichen, um einen bedeutenden Einfluss zu entwickeln. Wie eben gesehen, scheint die von Twitter gewählte Sprache hierbei ein guter Indikator zu sein.
tweets_eng <- tweets %>% filter(tweet_language == "en")
users_eng <- users %>% filter(userid %in% tweets_eng$userid)
# Deutlicher Nachlass in Tweets, 8,5 Mio -> 3 Mio (1/3), kaum Nachlass in Accounts mit mind. 1 englischsprachigen Tweet, 3608 -> 3077
tweets_eng1 <- tweets %>% filter(tweet_language == "en" & userid %in% lang_excl$userid[lang_excl$eng_perc >= 50])
users_eng1 <- users %>% filter(userid %in% tweets_eng1$userid)
# Deutlicher Nachlass in Accounts, kaum Nachlass in Tweets -> wenig genutzte, "bilinguale" Accounts
diff <- tweets_eng[!(tweets_eng$tweetid %in% tweets_eng1$tweetid), ] %>% 
  select(userid, account_creation_date, tweet_time) %>% mutate(tweet_time = as.Date(tweet_time))
diff.long <- melt(diff, id.vars = "userid")
diff.long %>% ggplot(aes(x = value, fill = variable)) + theme_minimal() +
  geom_histogram(bins = 100, position = "dodge") + labs(x = "Datum", y = "Anzahl") +
  scale_fill_discrete(name = "", labels = c("Account-\nErstelldatum", "Posting-Datum")) +
  theme(legend.position = "top")
# ABER: Strukturelles Posting der "gering"-Accounts Mitte/Ende 2014 könnte inhaltlich relevant sein. Auf Verdacht also in den Daten lassen, und für Analysen Nutzernamen dieser Spike speichern:
data.frame(sort(table(diff$tweet_time), decreasing = T))[1:20,]
spike <- diff %>% filter(tweet_time >= "2014-07-01" & tweet_time < "2014-10-01")
spike_usr <- unique(spike$userid)

write_csv(spike_usr, file.path("Y:/Twitter Bachelor/Other Files/Spike_Users.csv"), na = "NA", append = FALSE, 
          col_names = T, quote_escape = "double")
# Daten laden: spike_usr <- read_csv("Y:/Twitter Bachelor/Other Files/Spike_Users.csv")

# save(spike_usr, file = "Other Files/Spike_Users.RData")
# DATEN LADEN: load("Other Files/Spike_Users.RData")

rm(tweets, users, tweets_eng1, users_eng1, lang_excl, diff, diff.long, spike, spike_usr)



### Orte
table(users_eng$user_reported_location)
#Keine eindeutige Sortierung - wie zu vermuten bei individuellen Angaben - also manuelle Nachsortierung notwendig! (Im Folgenden hinter {Klammern} versteckbar für bessere Übersicht und einfache Code-Ausführung)
users_eng <- users_eng %>% mutate(shortened_location = NA)
{
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("U.S.A", "USA", "Usa", "usa", "US", "us", "united states", "United States", "America", "AMERICA", "Amerika", "THE US", "mother America", "Murica", "Estados Unidos", "Douglas, United States"), "US", NA)
  # US-Bundesstaaten und D.C.
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Troy, Alabama", "alabama", "AL"), "US, Alabama", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Alaska"), "US, Alaska", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Arizona, USA", "Arizona", "arizona", "ARIZONA", "City of Phoenix, Arizona", "Phoenix", "Mesa", "AZ"), "US, Arizona", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("arkansas"), "US, Arkansas", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Venice, California", "Redwood City, California", "San-Francisco", "Oakland, CA", "California", "California, USA", "CA", "UC Davis", "Santa Barbara, California", "San Francisco", "San Francisco, CA","SF", "San Diego", "San DIego, CA", "San Diego, United States", "Rancho Rinconada, CA, USA", "San Diego, CA", "Riverside", "Santa Monica", "Los Gatos, California", "Los Angeles, CA", "Los ANgeles", "los angeles", "Los-Angeles", "Los Angeles", "Hollywood", "Fowler, California"), "US, California", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("La Junta, Colorado", "Denver, CO", "Colorado"), "US, Colorado", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("New Haven"), "US, Conneticut", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Delaware, USA"), "US, Delaware", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Weeki Wachee, Florida", "USA, FL", "North Port, FL", "Destin, Florida", "Orlando", "Orlando, FL", "Tallahassee", "Miami", "Miami, FL", "Miami, USA", "jacksonville", "Jacksonville", "Florida, USA", "Florida"), "US, Florida", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("USA, Atlanta", "ATL", "ATL, GA", "Atlanta", "Atlanta, GA", "Atlanta, Georgia", "Macon, GA", "Georgia", "Georgia, USA", "Druid Hills, GA", "Downtown, Atlanta", "Brookhaven, GA"), "US, Georgia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c(), "US, Hawaii", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("ID"), "US, Idaho", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Aurora, Illinois", "Chicago", "Chicago, IL"), "US, Illinois", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("INDIANAPOLIS, USA"), "US, Indiana", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c(), "US, Iowa", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Wichita, KS", "Kansas", "Kansas, USA"), "US, Kansas", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Louisville"), "US, Kentucky", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Baton Rouge, LA", "New Orleans, LA", "New Orleans", "New Orlean", "Louisiana", "LA", "Lafayette, LA"), "US, Louisiana", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Brunswick, ME, USA"), "US, Maine", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Baltimore", "Baltimore, MD", "temple hills, md"), "US, Maryland", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Salem, Massachusetts", "Boston", "Boston, MA", "Boston, USA"), "US, Massachusetts", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Bloomfield Hills, Michigan", "Michigan", "Flint, MI", "Detroit", "Detroit, Michigan"), "US, Michigan", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("MN", "Mn", "Minnesota", "Minneapolis, MN", "Menisotta"), "US, Minnesota", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Jackson, MS", "Jackson"), "US, Mississippi", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("StLouis", "St Louis, MO", "Missouri, USA", "Kansas City, MO", "Ferguson, MO"), "US, Missouri", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Montana"), "US, Montana", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Nebraska, USA"), "US, Nebraska", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Vegas", "Nevada", "Las Vegas"), "US, Nevada", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("NH", "New Hampshire"), "US, New Hampshire", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("NJ", "Newark, NJ", "Camden, NJ", "New Jersey", "New Jersey, USA", "Old Bridge, New Jersey"), "US, New Jersey", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("NM", "Albuquerque", "Albuquerque, NM", "Eldorado at Santa Fe, NM, USA", "Los Alamos"), "US, New Mexico", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("NYC", "nyc", "NY City", "NY", "ny", "New York, USA", "New York, NY", "New York City", "New - York", "New-York", "Bronx, NY", "brooklyn", "Brooklyn", "Brooklyn, NY", "Brkln", "The Big Apple", "Queens, NY", "New York", "Manhattan, NY", "Garden City, NY", "Johnson City, New York", "East Aurora, New York", "Baldwinsville, New York", "Buffalo"), "US, New York", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("North Carolina", "Raleigh, North Carolina", "Greensboro"), "US, North Carolina", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("North Dakota"), "US, North Dakota", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Whitehall, Ohio", "Cincinnati, OH", "Cleveland, OH", "cleveland / ohio", "Cleve", "Columbus, Ohio", "Ohio", "Ohio, USA", "OH", "Montgomery", "Montgomery, Ohio", "Millville village, OH, USA", "Milford, Ohio", "Grove City, Ohio", "Green village, OH, USA", "City of Cleveland, USA", "Cincinnati"), "US, Ohio", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Oklahoma", "Oklahoma City", "Oklahoma, PA", "Oklahoma, USA"), "US, Oklahoma", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("portland", "Portland"), "US, Oregon", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Pittsburgh", "Pittsburgh, PA", "Pittsburgh, US", "Philadelhia", "Philadelphia", "Philadelphia, PA", "Philly", "Pennsylvania", "Mohnton, PA", "Chester, PA"), "US, Pennsylvania", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Rhode Island", "Rhode island"), "US, Rhode Island", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("South Carolina, USA", "Columbia, SC", "Charleston, SC"), "US, South Carolina", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c(), "US, South Dakota", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Tennessee, USA", "TN", "Nashville", "Memphis", "Memphis, TN"), "US, Tennessee", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Texas, USA", "Texas", "Austin", "Austin, TX", "Ostin", "All Over Texas", "City of San Antonio, TX", "Dallas", "Dallas, Texas", "Stonewall, TX", "Houston", "Houston, TX", "El Paso, Texas", "Dallas, TX"), "US, Texas", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Utah", "utah", "Salt Lake City"), "US, Utah", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c(), "US, Vermont", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Winchester, Virginia", "Richmond, VA"), "US, Virginia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("WA", "Washington", "Seattle, WA", "Seattle", "Stanwood city, WA, USA"), "US, Washington", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c(), "US, West Virginia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Wisconsin, USA", "Milwaukee", "Milwaukee, WI", "Madison"), "US, Wisconsin", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c(), "US, Wyoming", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Washington D.C", "Washington D.C.", "Washington, D.C.", "Washington, DC"), "US, Washington D.C.", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Guanica zona urbana, PR, USA"), "US, Unincorporated", users_eng$shortened_location)
  # Russland
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Krasnoyarsk", "Krasnoyarsk, Russia", "Ekaterinburg, Russia", "Chelyabinsk, Russia", "Volgograd", "Velikiy Novgorod, Russia", "Stavropol, Russia", "Ufa, Russia", "Rostov-na-Donu, Russia", "russia", "Russia", "Russian Empire", "novgorod", "Tomsk", "Republic of Chechnya, Russia", "Crimea, Russia", "Perm, Russia", "Penza", "Omsk, Russia", "Nizhniy Novgorod, Russia", "Murmansk, Russia", "Irkutsk", "Chelyaba", "belgorod", "✴Новгород✴", "Уфа", "Ульяновск", "Тула, Тульская область", "уфа", "ул. Ленина", "ростов-я-тону", "россия", "омск", "новосибирск", "новгород", "екб", "днищний дновгород", "Ярославль, Россия", "Ярославль", "Чита", "Чеченская республика, Россия", "Челябинск", "Челны", "Чебоксары, Россия", "Чебоксары", "Ханты-Мансийск", "Хабаровск, Россия", "Хабаровск", "Тула, Россия", "Тула", "Тува", "Томск", "Тольятти", "Тверь", "Тверское подворье", "Ташла", "Тамань", "Сраратов", "Сочи, Россия", "Сочи", "Смоленск", "Симферополь", "Саров", "Саратов )))", "Саратов", "Саранск", "Самара", "Рязань", "Ростов-на-Дону, Россия", "Ростов-на-Дону", "Ростов-На-Дону"), "Russia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Ростов", "Россия, Казань", "Россия", "Российская Федерация", "РФ", "Пятигорск", "Пенза", "Орел", "Омск, Россия", "Омск", "Нягань", "Мурманск", "Магадан", "Люберцы", "Луга", "Липецк", "Курск", "Красноярск", "Киров", "Кемерово", "Петрозаводск", "Пермь", "овосибирск, Россия", "Новосибирск", "Новосиб", "Новокузнецк, Россия", "Новгород", "Новосибирск, Россия", "Краснодар", "Котлас", "Кострома", "Кольский п-в", "Новосибирск, Россия", "Нижний Новогород", "Нижний Новгород", "Нижний", "Екатеринбург...", "Екатеринбург", "ЕКБ", "Ё-бург", "Нижний Новгород", "Нижний", "Набережные Челны, Россия", "Калуга, Россия", "Калуга", "Казань", "Калининград, Россия", "Калининград", "Иркутск", "Иваново", "Елец", "Барнаул, Россия"), "Russia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c( "Грозный", "Грозный", "Вся Россия", "Вся Россия", "Воронеж, Россия", "Воронеж", "Вологда", "Волгоград, Россия", "Волгоград, Вася!", "Волгоград", "Волгодонск, Россия", "Великий Новгород", "Брянск", "Белгород", "Башкартостан", "Архангельск, Россия", "Архангельск", "☼Новосибчик☼"), "Russia", users_eng$shortened_location)
  # Aufsplittung in mehrere Befehle umgeht komische Sytax-Fehler
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Russia, Moscow", "moscow city", "Moscow-City", "Moscow", "msk", "Msk", "MSK", "MSK SAO", "★москва", "Moscow, Russia", "moscow", "Москва♥", "Москоу-сити", "осква, Россия", "Москва Златоглавая", "Москва Белокаменная", "Москва (СССР - Россия)", "Москва - столица", "Москва - Самара", "Москва - Лондон", "Москва", "Моска", "мск", "москва", "Туапсе, Москва", "Серпухов", "Севастополь", "Россия, Москва", "Пушкино", "Псков", "Подольск", "Одинцово", "Москва, Россия", "МСКВА", "МСК", "Коломна", "Перово", "МSK", "Дмитров"), "Russia, Moscow", users_eng$shortened_location) # Moskau-Stadt + Oblast
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("St-Petersburg", "St. Petersburg", "st.petersburg", "St.Petersburg", "st/petersburg", "Saint Petersburg, Russia", "Saint Petersburg", "saint-petersburg", "Saint-Peterburg", "saint P.", "saint p.", "Saint-P.", "Saint-P", "St.P", "St.P.", "St-P", "SPb♥○•°•☆", "SPB", "SPb", "spb", "cанкт-петербург", "спб", "Спб", "Сестрорецк, Россия", "Санкт-петербург", "Санкт-Петербург, Россия", "Россия Санкт-Петербург", "Санкт-Петербург", "СПб", "СПБГПУ", "СПБ", "С.Петербург", "С-Пб", "Пушкин, Санкт-Петербург", "Пушкин", "ПЕТЕРБУРГ", "Лос-Питербургос", "Ленинград", "Кронштадт", "Кингисепп", "Петроград", "Петербург", "Выборг"), "Russia, St.Petersburg", users_eng$shortened_location) # St.Petersburg + Oblast Leningrad
  # Sonstige Länder
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Kiel, Schleswig-Holstein", "Köln, Deutschland", "Hessen, Deutschland", "Hamburg, Deutschland", "Frankfurt am Main, Deutschland", "Frankfurt am Main, Hessen", "Erfurt, Deutschland", "Düsseldorf, Deutschland", "Dresden, Sachsen", "Bremen, Deutschland", "Berlin, Deutschland", "Stuttgart, Deutschland", "Rostock, Deutschland", "Saarbrücken, Deutschland", "Deutschland", "München, Bayern", "Magdeburg, Deutschland", "Köln, Deutschland", "Потсдам", "Germany"), "Germany", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("UK", "Newcastle", "Newport", "Manchester", "London", "London, England", "London, UK", "Liverpool", "Coventry", "United Kingdom"), "United Kingdom", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Italy", "Italia", "italia", "Milano, Lombardia", "Itala, Sicilia"), "Italy", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Paris", "Paris, France", "Lyon", "France"), "France", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Greece"), "Greece", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Brussel, België"), "Belgium", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Стокгольм"), "Sweden", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Украина", "Покровское", "Одесса", "Мариуполь", "Луцк", "Луганск", "Кременчуг", "Київ", "Киев", "Запорожье", "Житомир", "Донецк, Россия", "Донецк", "Днепр, Украина", "Днепр"), "Ukraine", users_eng$shortened_location) # Krim zu Russland, Donetsk zu Ukraine
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Таллин"), "Estonia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Прага"), "Czechia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Мінск", "Витебск", "Беларусь"), "Belarus", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Стамбул"), "Turkey", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Mexico"), "Mexico", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Cairo, Egypt", "مصر", "Egypt"), "Egypt", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Beirut", "لبنان", "بيروت"), "Lebanon", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("العراق"), "Iraq", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Kuwait"), "Kuwait", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("баку"), "Azerbaijan", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("syria", "Syria", "Damascus", "Aleppo", "سورية", "دمشق", "سوريا", "حماة", "حمص"), "Syria", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Oliva Pizza & Pasta // Amman", "Zarqa, Jordan", "Amman", "Az-Zarqa", "حلب", "اللاذقية", "المملكة الأردنية الهاشمية", "الأردن"), "Jordan", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("الدمام"), "Saudi-Arabia", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Манила"), "Philippines", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Manama, Bahrain"), "Bahrain", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("عمان"), "Oman", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Пхеньян"), "North Korea", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("katamandu"), "Nepal", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("ZM"), "Zambia", users_eng$shortened_location)
  # Other
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Universe", "Wonderland", "sin city", "Love Town", "Liberty City", "Fattyland", "garage", "hood", "I AM A CITIZEN OF THE UNIVERSE", "Islamic States of America", "dreamland", "Black America", "хогвортс", "Cosmopolitanism", "2148", "الدولة الإسلامية", "мой мир", "амфетаминовая столица", "Я везде", "Черемухи", "ЦАО", "Третий Рим",  "Раша", "Работа))", "Мечтовиль", "Мечта", "Красти Бургер", "Артем", "#РусскийМир"), "Other", users_eng$shortened_location)
  users_eng$shortened_location <- ifelse(users_eng$user_reported_location %in% c("Newhustle", "vladik", "Man", "piter", "Jersey", "KN", "Birmingham", "Cromwell", "ширкино", "Ch", "сжигай толерантный рай,слышишь", "сейчас орел", "Таллин, Санкт-Петербург", "Сибирь матушка", "Северная столица", "♠Питер♠", "питер", "Питер", "Новгород - СПб", "НН", "Владик ДВФУ", "Вильнюс - Москва", "Ватноград", "Бутово", "Адлер", "Adler"), "Unidentified", users_eng$shortened_location)
  users_eng$shortened_location[2464] <- "Russia"
}
sum(is.na(users_eng$shortened_location))
table(is.na(users_eng$shortened_location) == is.na(users_eng$user_reported_location)) # keine neuen Missings

table(users_eng$shortened_location)

length(grep("US", users_eng$shortened_location)); length(grep("Russia", users_eng$shortened_location))
length(grep("US,", users_eng$shortened_location))
length(grep("US,", users_eng$shortened_location)) / length(grep("US", users_eng$shortened_location)) *100
sort(table(users_eng$shortened_location[grep("US,", users_eng$shortened_location)]), decreasing = T)
# Knapp 44% der US-Amerikaner haben eine genauere Ortsangabe als nur "USA" oder vergleichbares getätigt, es lässt sich jedoch keine politische Tendenz (Republikanisch/Demokratisch bzw. Swing States) erkennen.
sort(table(users_eng$shortened_location[grep("Russia", users_eng$shortened_location)]), decreasing = T)
# Interessanterweise Konzentrierung in russischen Daten: Fast genausoviele Angaben für Moskau wie für Russland generell, halb so viele Angaben für St.Petersburg



### Account-Erstelldaten
min(tweets_eng$account_creation_date); max(tweets_eng$account_creation_date)
# Beinahe 9 Jahre an Accounts und Tweets finden sich in den Daten
quartals <- c(as.Date("2009-01-01"), as.Date("2009-04-01"), as.Date("2009-07-01"), as.Date("2009-10-01"))
for(i in 10:18){
  year <- as.character(2000 + i)
  for(q in c("-01-01", "-04-01", "-07-01", "-10-01")){
    quartals <- append(quartals, as.Date(paste(year, q, sep ="")))
  }
}
rm(i, q, year)
quartals <-  quartals[1:39]

ggplot(users_eng, aes(x = account_creation_date, fill = account_language)) + geom_histogram(breaks = quartals) + 
  scale_fill_manual(name = "Sprache", values = c("#189159", "#BEA310", "#F8766D", "#D35130", "#915123", "#BE6D10",
                                                 "#4D9E22", "#AE8046", "#619CFF", "#4A3FC6", "#00BA38"),
                    labels = c("Arabisch", "Deutsch", "Englisch", "Englisch (GB)", "Spanisch", "Französisch",
                               "Indonesisch", "Italienisch", "Russisch", "Ukrainisch", "Chinesisch")) +
  labs(x = "Quartal", y = "Anzahl Accounts") + theme_minimal()
# Der Großteil der Accounts wurde im Zeitraum 2. Häfte 2013 - 1. Häfte 2014 erstellt - lange vor dem US-Wahlkampf 2016, und sowohl für englische als auch für russische Accounts. Mögliche Erklärungen: Zeitnutzung, um Accounts als "seriös" zu etablieren, oder Nutzung der Accounts zur Beeinflussung anderer Themen als der Wahl.
table(users_eng$account_creation_date < "2013-06-01")
table(users_eng$account_creation_date > "2014-06-01")
(1 - (98 + 1103) / nrow(users_eng)) *100
# 98 Accounts wurden vor und 1103 nach dieser Spitze erstellt, 1876 bzw. 61% in dieser Spitze.
min(users_eng$account_creation_date); max(users_eng$account_creation_date)

rm(quartals)


### Account-Aktivitäten
activity <- data.frame(userid = users_eng$userid, cration = users_eng$account_creation_date, tweets = 0)
for(i in 1:nrow(users_eng)){
  twe <- tweets_eng %>% filter(userid == users_eng$userid[i])
  if(nrow(twe) != 0){
    activity[i, 3] <- nrow(twe)
    activity[i, 4] <- as.Date(min(twe$tweet_time))
    activity[i, 5] <- as.Date(max(twe$tweet_time))
  }
} # This might take a while
names(activity) <- c("userid", "creation", "tweets", "first.post", "last.post")
rm(twe, i)
activity <- activity %>% mutate(sleep = as.integer(first.post - creation), 
                                active = as.integer(last.post-first.post + 1))
summary(activity$sleep)
summary(activity$active)
ggplot(activity, aes(x = active)) + geom_histogram(bins = 75) + 
  scale_x_continuous(trans = "sqrt", breaks = c(10, 100, 250,500, 1000, 1500, 2000, 2500, 3000)) +
  labs(x = "Tage an Aktivität des Accounts", y = "Anzahl") + theme_minimal()
# Dominante Menge an Accounts sind nur ein, zwei Tage aktiv. Nur minimaler Anteil an Accounts sind über 1.000 Tage aktiv
ggplot(activity, aes(x = sleep)) + geom_histogram(bins = 75) +
  scale_x_continuous(trans = "sqrt", breaks = c(10, 100, 250,500, 1000, 1500, 2000, 2500, 3000)) +
  labs(x = "Tage seit Account-Erstellung bis zu erstem Post", y = "Anzahl") + theme_minimal()
# Deutlich weniger Klarheit in Inaktivität vor erstem Post. Viele Accounts sind sofort aktiv, aber es gibt Gruppen mit lokalen Maxima um 250, 500 und 800 Tage Inaktivität



ggplot(activity, aes(x = active, y = sleep, color = tweets)) + geom_point() + theme_minimal() +
  scale_x_continuous(trans = "sqrt", breaks = c(10, 75, 200, 500, 1000, 2000, 3000)) + 
  scale_y_continuous(trans = "sqrt", breaks = c(10, 75, 200, 500, 1000, 1500, 2000)) + 
  scale_color_binned(trans = "sqrt", low = "blue", high = "red", breaks = c(20, 500, 3000, 12000, 30000),
                     name = "Anzahl an\nTweets") +
  labs(x = "Tage an Aktivität des Accounts", y = "Tage seit Account-Erstellung bis zu erstem Post")

as.Date_origin <- function(x){as.Date(x, origin = '1970-01-01')}
ggplot(activity, aes(x = active, y = sleep, color = creation)) + geom_point() + theme_minimal() +
  scale_x_continuous(trans = "sqrt", breaks = c(10, 75, 200, 500, 1000, 2000, 3000)) + 
  scale_y_continuous(trans = "sqrt", breaks = c(10, 75, 200, 500, 1000, 1500, 2000)) + 
  scale_colour_gradientn(colours=rainbow(9), labels=as.Date_origin, name = "Erstelldatum", n.breaks = 6) +
  labs(x = "Tage an Aktivität des Accounts", y = "Tage seit Account-Erstellung bis zu erstem Post")
# Über lange Zeiträume genutzte Accounts wurden meist direkt nach Ertstellung aktiv, während Accounts, die nur wenige Tage benutzt wurden eher lange auf diese kurze Aktivität "warteten" Zudem lässt sich ein beinahe viereckiger Kasten aus Accounts bis 1000 Tage Aktivität und bis 600 Tage vor erstem Post erkennen. Diese Beobachtung ist insbesondere einer Gruppe an Acocunts, die um die 500 Tage nach Erstellung aktiv wurden und in Aktivität sowie Anzahl abgesetzter Tweets stark schwanken, zu verdanken.
# Zusätzlich lässt sich erkennen, dass Accounts mit längerer Aktivitätszeit auch im Schnitt mehr Tweets absetzen, was von einer konstanten Aktivität ausgehen lässt.
# Zusätzlich lässt sich festhalter, dass neuere Accounts im Schnitt nur wenige Tage vor ihrem ersten Post inaktiv sind.
rm(activity, i, as.Date_origin)



### Gefolgte Accounts
hist(users_eng$following_count)
hist(log2(users_eng$following_count))
summary(users_eng$following_count)
# Ein Großteil der Accounts folgt nur ein paar Hundert Accounts (wenn überhaupt), während einige wenige Accounts mehreren zehntausend Accounts folgen -> Wahrscheinlich Nutzung von Follow-Bots, um eigene Nummern zu erhöhen.

# Suche nach Cutoff-Punkt, Umschwung natürliches Wachstum -> Followbots
lm_seq <- seq(500, 5000, 100)
lm_df <- data.frame(n = lm_seq, adr_follow = 0, adr_no = 0, rem = 0)
for(i in 1:length(lm_seq)){
  followbots <- users_eng[which(users_eng$following_count >= lm_seq[i]), ]
  followbots <- followbots[, 7:8]
  lm_df$rem[i] <- nrow(followbots)
  lm <- summary(lm(followbots$follower_count ~ followbots$following_count))
  lm_df$adr_follow[i] <- lm$adj.r.squared
  nobots <- users_eng[which(users_eng$following_count < lm_seq[i]), ]
  nobots <- nobots[, 7:8]
  lm <- summary(lm(nobots$follower_count ~ nobots$following_count))
  lm_df$adr_no[i] <- lm$adj.r.squared
}
rm(followbots, nobots, i, lm_seq, lm)
lm_df %>% ggplot(aes(x = n)) + geom_line(aes(y = adr_follow, color = "Follows >= N")) + 
  geom_line(aes(y = adr_no, color = "Follows < N")) +
  labs(x = "Zahl eigener Follows (N)", y = "adj. R^2", color = "") + theme_minimal()
lm_df$n[which(lm_df$adr_follow == max(lm_df$adr_follow))]
# Ab 2.800 eigenen Follows ist davon auszugehen, dass Accounts followbots nutzen, da an diesem Punkt die Varianzaufklärung maximiert ist. Der relativ lineare Abfall für N > 2800 lässt sich vermutlich dadurch erklären, dass tatsächliche Followbot-Nutzer in die Gruppe der Nicht-Nutzer einsortiert werden.
followbots <- users_eng[which(users_eng$following_count >= 2800), ]
followbots <- followbots[, c(1, 7:9)]
nobots <- users_eng[which(users_eng$following_count < 2800), ]
nobots <- nobots[, c(1, 7:9)]
summary(lm(followbots$follower_count ~ followbots$following_count))
summary(lm(nobots$follower_count ~ nobots$following_count))
# Für die Accounts, die über 2.800 anderen Accounts folgen, besteht ein deutlicher Zusammenhang zwischen der Anzahl gefolgter Accounts und der Anzahl eigener Follower. Während für die 2.895 Accounts unter 2.800 Follows die Anzahl gefolgter Accounts nur knapp über 1% der Varianz in den eigenen Follower-Zahlen erklärt (adj. R^2 = 0,011), ist es für die Accounts mit über 2.800 Follows eine Varianzaufklärung von über 70% (adj. R^2 = 0,71)! Zusätzlich ist für beide Account-Gruppen ersichtlich, dass sie ab mehreren hundert Followern im Schnitt mehr Follower haben, als sie selbst followen, da in beiden Fällen für jeden Follow 1,5 bzw. 1,6 eigene Follower hinzukommen.
summary(users_eng$follower_count)
sum(users_eng$follower_count < 10)
sum(users_eng$follower_count < 100)
table(follow_df$bots[follow_df$followers >= 1000])
table(follow_df$bots[follow_df$followers >= 1933])

rm(followbots, nobots, lm_df)


### Tweet-Zeiten
times <- tibble(dt = tweets_eng$tweet_time %>% ymd_hms()) %>%
  mutate(timepart = hms::hms(as.numeric(times$dt - floor_date(times$dt, "1 day"), unit="secs")), 
         timeset = as.integer(substr(timepart,1,2))) %>%  mutate(timeset_ru = timeset + 3) %>% 
  mutate(timeset_ru = ifelse(timeset_ru >23, timeset_ru - 24, timeset_ru))
# Will definitely take time
times_plot <- data.frame(time_ru = sort(unique(times$timeset_ru)), tweetnum = 0)
for(i in 1:24){
  df <- times %>% filter(timeset_ru == times_plot$time_ru[i])
  times_plot$tweetnum[i] <- nrow(df)
}

ggplot(times_plot, aes(x = time_ru, y = (tweetnum-median(tweetnum)))) + 
  geom_col(fill = "#4A3FC6") + theme_minimal() + 
  labs(x = "Uhrzeit Westrussland (MSK, in Stunden)", y = "Anzahl Tweets (Median-Normalisiert)")
# Manuelle Einfügung der Uhrzeiten in anderem Grafikprogramm
rm(times, times_plot)


### Tweets vs. Retweets
table(tweets_eng$is_retweet)

tweet_rts <- tweets_eng %>% select(userid, is_retweet, retweet_userid)
user_rts <- users_eng %>% select(userid, follower_count)

for(i in 1:nrow(users_eng)){
  id <- users_eng$userid[i]
  df <- tweet_rts %>% filter(userid == id)
  rt <- df %>% filter(is_retweet == TRUE)
  user_rts[i, 3] <- nrow(df)
  user_rts[i, 4] <- nrow(rt)
} # kann einen kurzen Moment dauern
user_rts[, 5] <- user_rts[, 4] / user_rts[, 3]
names(user_rts) <- c("userid", "followers", "postcount", "retweets", "rtpercent")

# Anteil Retweets an allen Postings eines Users
user_rts %>% ggplot(aes(x = rtpercent, y = ..density..)) + geom_histogram(bins = 100, fill = "#4A3FC6") +
  geom_density(color = "red", size = 1) + labs(x = "Anteil Retweets", y = "Dichte (density)") +
  theme_minimal() + scale_y_continuous(trans = "sqrt")
#Die große Mehrheit der Accounts setzt entweder auf eigene Postings und wenige bis gar keine Retweets oder quasi nur auf Retweets. Auffällig sind auch einzelne Spikes und eine Ansammlung an Accounts um die 85-95% Retweet-Rate.

# Anteil Retweets an allen Postings eines Users - nach Followern
user_rts %>% ggplot(aes(x = followers, y = rtpercent, color = postcount)) + geom_point() +
  scale_colour_gradientn(trans = "sqrt", colours=rainbow(9), name = "Anzahl\nPostings", 
                         breaks = c(2000, 10000, 25000, 50000, 100000), labels = scales::comma) +
  scale_x_continuous(name = "Follower-Zahl", labels = scales::comma, trans = "sqrt", 
                     breaks = c(1000, 10000, 25000, 50000, 100000, 200000)) +
  geom_smooth (alpha=0.15, size=0, span=0.5) +
  stat_smooth (geom="line", alpha=0.5, size=1, span=0.5) +
  labs(y = "Anteil Retweets") + theme_minimal()
# Accounts mit vielen Followern (fünfstellig und aufwärts) setzen hauptsächlich auf eigene Tweets und retweeten wenig. Mit zunehmender Follower-Zahl gent die Anzahl an Retweets weiter zurück.
user_rts %>% ggplot(aes(x = followers, y = rtpercent, color = postcount)) + geom_point() +
  scale_colour_gradientn(trans = "sqrt", colours=rainbow(9), name = "Anzahl\nPostings", 
                         breaks = c(2000, 10000, 25000, 50000, 100000), labels = scales::comma) +
  scale_x_continuous(name = "Follower-Zahl", labels = scales::comma, trans = "log", 
                     breaks = c(1, 10, 100, 1000, 10000, 100000)) +
  geom_smooth (alpha=0.15, size=0, span=0.5) +
  stat_smooth (geom="line", alpha=0.5, size=1, span=0.5) +
  labs(y = "Anteil Retweets") +theme_minimal()
# Es zeigt sich ein leichter Unterschied in der Follower-Zahl zwischen den Gruppen mit beinahe gar keinen Retweets und der Gruppe mit fast ausschließlich Retweets. Während sich im Bereich 10-100 eine Gruppe an Accounts mit nahe 0 Retweets findet, existiert eine ähnliche Gruppe mit >90% Retweets im Bereich um die 1.000 Follower.

# Erkenntnis 1:Retweet-Accounts scheinen im Schnitt "beliebter" gewesen zu sein als Accounts mit eigenen Postings. Aber was retweeten diese Accounts? Sind es Tweets anderer IRA-Accounts, oder sind es Tweets "echter" Akteure?
table(tweet_rts$retweet_userid[tweet_rts$is_retweet == T] %in% users_eng$userid)

# Erkenntnis 2: Der Account, der mit Abstand die meisten Postings veröffentlicht hat, hat dies zu großen Teilen durch Retweets bewerkstelligt. Die nächstgrößeren Accounts mit 50.000+ Posts retweeten dagegen jedoch kaum bzw. zu großen Teilen quasi gar nicht. -> Nachrichtenseiten, die eigene Artikel teilen?
ids <- head(users_eng$userid[order(users_eng$follower_count, decreasing = T)], 20)
tibble(names = sapply(ids, function(id){users_eng$user_screen_name[users_eng$userid == id]}, USE.NAMES = F), 
       tweets = sapply(ids, function(id){user_rts$postcount[user_rts$userid == id]}, USE.NAMES = F), 
       follower = head(users_eng$follower_count[order(users_eng$follower_count, decreasing = T)], 20))
# Nein, hauptsächlich russisch-namige Accounts mit wenigen Tweets, und einige zumindest nicht offensichtlich russische Accounts mit vielen Tweets
rm(df, rt, tweet_rts, user_rts, i, id, ids)



# Cleanup Data Sets ----

### Entfernung nicht berücksichtigter Informationen

# Retweets: Retweets sind zwar für eine Netzwerkanalyse ineressant, für die hier im folgenden angewendete Sprachprozessierung jedoch nicht wirklich hilfreich, da ein einzelner Tweet unter Umständen durch Retweets mehrere hundert Male im Datensatz vorkommen und so die Klassifizierung beeinflussen könnte.
tweets_eng <- tweets_eng %>% filter(is_retweet == FALSE)
# Eine Million Tweets weniger

### Entfernung unnötiger Klammern und leerer Einträge in bestimmten Spalten
tweets_eng$hashtags <- str_replace_all(tweets_eng$hashtags, "\\[", "")
tweets_eng$hashtags <- str_replace_all(tweets_eng$hashtags, "\\]", "")
tweets_eng$hashtags[tweets_eng$hashtags==""] <- NA

tweets_eng$urls <- str_replace_all(tweets_eng$urls, "\\[", "")
tweets_eng$urls <- str_replace_all(tweets_eng$urls, "\\]", "")
tweets_eng$urls[tweets_eng$urls==""] <- NA

tweets_eng$user_mentions <- str_replace_all(tweets_eng$user_mentions, "\\[", "")
tweets_eng$user_mentions <- str_replace_all(tweets_eng$user_mentions, "\\]", "")
tweets_eng$user_mentions[tweets_eng$user_mentions==""] <- NA

# Getaggte Nutzer: Viele der Tweets taggen andere Nutzer per @NUTERNAME. Da diese Information auch über die Variable "user_mentions" in den Daten vorhanden ist und die Nutzernamen unter Umständen die Textanalyse des Topic Models beeinflussen, werden sie zu Beginn entfernt. Auch Hashtag-Symbole können entfernt werden, da verwendete Hashtags separat in einer eigenen Variable getrackt werden.
tweets_clean <- tweets_eng
tweets_clean$tweet_text <- gsub("@[a-zA-Z0-9_]*", "", tweets_clean$tweet_text)
tweets_clean$tweet_text <- gsub("#", "", tweets_clean$tweet_text)

### Verwendete Medien: Viele der Nutzer verknüpfen ihre Posts mit Bild- oder Videomedien oder Links zu anderen Webinhalten. Diese werden innerhalb des Tweet-Textes als abgekürzter Link (https://t.co/...) dargestellt. Da diese Medien und externen Verlinkungen bei der hier durchgeführten Analyse nicht beachtet werden, können sie entfernt werden.
tweets_clean$tweet_text <- gsub("https?://t.co/[a-zA-Z0-9]*", "", tweets_clean$tweet_text)



### Emoji
# Viele der Tweets beinhalten Emoji. Diese können vom stm-Textprozessor nicht bearbeitet werden, da sie zwar technisch als Zahlen- und Buchstabenkombinationen angegeben werden, eine korrekte Verarbeitung jedoch nicht gewährleistet werden kann. Zusätzlich dazu ist es in vielen Tweets der Fall, dass Emoji untereinander bzw. Emoji und tatsächliche Worte nicht durch Leerstelen getrennt werden. Diese Tatsache führt dazu, dass der Gesamtverbund aus Emoji und Wort als Texteinheit etabliert wird und somit beispielsweise "☑️wort" und "wort" als grundverschiedene Einheiten erfasst werden. Das führt dazu, dass beispielsweise ein Topic-definierendes Wort ohne die Entfernung der Emoji "💥eraseobama���are���" war. Eine Umbenennung der Emoji in Text war demnach eindeutig vonnöten.
#Um dies zu beheben wurde auf Basis der offiziellen Emoji-Liste des Unicode-Konsortiums (https://www.unicode.org/emoji/charts/full-emoji-list.html, aufgerufen und erstellt am 30.03.2020) ein Datensatz erstellt, der die Emojinummer, das entsprechende Browser-Emoji und den jeweiligen offiziellen Kurznamen sowie die Anzahl der für jedes Emoji verwendeten Symbole beinhaltet. Diese Kurznamen wurden als Grundlage für die Text-Ersetzungen genommen. Die Voranstellung von "emoj_" an jeden der Begriffe sorgt dabei dafür, dass jedes Emoji auch in Textform klar erkennbar bleibt. Die Entfernung jeglicher Leerstellen und Sonderzeichen sorgt dafür, dass jedes Emoji als ein einzelnes Wort behandelt wird.
emoji <- read_csv2("Other Files/emoji-list.txt", col_names = T, col_types = cols(code = col_character(), Replace = col_character()), locale = locale(encoding = "UTF-8"))
#Hinzufügen einer Leerstelle, um Emoji voneinander zu trennen, sollten mehrere direkt aufeinander folgen
emoji$Replace <- paste(" ", emoji$Replace, " ")
for(i in seq(1,length(emoji$Replace))){
  tweets_clean$tweet_text <- gsub(emoji$code[i], emoji$Replace[i], tweets_clean$tweet_text)
}# ACHTUNG: Berechnete Laufzeit: Mehrere Stunden  (1809 Loop-Iterationen über ~2 Mio. Strings mit variablen Längen)!
tweets_clean$tweet_text <- str_squish(tweets_clean$tweet_text)
# Entfernung von möglichen mehrfachen Leerstellen, um eventuellen Problemen zuvorzukommen.
rm(i, emoji)
# write_csv(tweets_eng, file.path("Twitter Data/tweets_en-norts.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")
# write_csv(tweets_clean, file.path("Twitter Data/tweets_cleaned.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")



### Das Problem der Duplikate ----
?duplicated # "smallest index" wird selbst nicht als Duplikat gezählt!
duplicates <- tweets_clean[duplicated(tweets_clean$tweet_text) | duplicated(tweets_clean$tweet_text, fromLast=TRUE), ]

length(unique(duplicates$tweet_text)) # 94.434 einzigartige Texte in 680.611 Tweets - Jeder Tweet im Schnitt 7 mal.
length(unique(duplicates$userid)) # Mehr als die Hälfte aller Nutzer haben mind. einen Duplikat-Tweet veröffentlicht.
# Auch ohne Retweets schaffen es eine bedeutende Menge an Tweets, mehrfach in den Daten aufzuauchen, da sie wortgleich mehrfach gepostet wurden. Zwar lassen sich diese relativ simpel entfernen, aber es finden sich zweifellos auch wortähnliche Tweets bzw. Tweets, die sich nur durch das Erwähnen bestimmter Namen unterscheiden, und somit von dieser Filterung nicht erfasst werden würden. Belässt man jedoch alle Duplikate in den Daten, so werden die Topics von diesen Duplikaten dominiert (exakte Kopien = perfekte Wortähnlichkeit, und somit Dominanten der jw. Topics).

# Kompromiss: Belassen von Duplikaten über bestimter Reichweite, Entfernung von Duplikaten mit kaum Reichweite. Auf diese Art und weise wird zwar das Verhalten und die thematischen Verteilungen der IRA-Accounts verändert, ihre Auswirkung auf andere Nutzer wird jedoch kaum vermindert.
duplicates <- duplicates %>% mutate(interactions = quote_count + reply_count + like_count + retweet_count)
sum(duplicates$interactions >= 1000); sum(duplicates$interactions >= 100); sum(duplicates$interactions >= 10)
# 100 erscheint als guter Cutoff-Punkt - 3.636 Tweets sollten auch als Duplikate die Topics nicht dominieren, und gleichzeitig genug Details über Duplikat-Themen in den Daten lassen.
duplicates <- duplicates[order(duplicates$interactions, decreasing = T), ] # Order, damit unique()/duplicated() immer den reichweitenstärksten Tweet als "Original" zählen
dupli_filtered <- duplicates[!(duplicated(duplicates$tweet_text)) | duplicates$interactions >= 100, ]
# Gefiltert zu allen Tweets, die entweder die reichweitenstärksten Duplikat-Tweets sind, oder Interaktionen im dreistelligen Bereich erhielten
ignored <- duplicates$tweetid[!(duplicates$tweetid %in% dupli_filtered$tweetid)]


tweets_clean <- tweets_clean[!(tweets_clean$tweetid %in% ignored),]
tweets_eng <- tweets_eng[!(tweets_eng$tweetid %in% ignored),]

# write_csv(tweets_eng, file.path("Twitter Data/tweets_en-norts-nodupes.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")
# write_csv(tweets_clean, file.path("Twitter Data/tweets_clean-nodupes.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")
rm(duplicates, dupli_filtered, ignored)

# DATEIEN LADEN: 
# tweets_eng <- read_csv("Twitter Data/tweets_en-norts-nodupes.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))
# tweets_clean <- read_csv("Twitter Data/tweets_clean-nodupes.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))




# STM - Vorbereitung ----

toks <- quanteda::tokens(tweets_clean$tweet_text,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_punct = TRUE)

toks <- tokens_remove(tokens_tolower(toks), c(stopwords("en"), "get", "go", "say"))
toks <- tokens_wordstem(toks)
dtm <- dfm(toks)
dtm <- dfm_trim(dtm, min_docfreq = 15) # Mindest-Worthäufigkeit, um beachtet zu werden
#dtm
topfeatures(dtm, n = 50)

docvars(dtm, "date") <- as.Date(tweets_clean$tweet_time)
docvars(dtm, "quotecount") <- tweets_clean$quote_count
docvars(dtm, "replycount") <- tweets_clean$reply_count
docvars(dtm, "likecount") <- tweets_clean$like_count
docvars(dtm, "retweetcount") <- tweets_clean$retweet_count

stm_dtm <- convert(dtm, to = "stm")
# Durch das Entfernen von Stopwords und vorangegangenes Cleaning werden ca. 2.300 der 1,3m Tweets leer (""). Diese können nicht für weitere Analysen verwendet werden und werden hiermit entfernt, was die auftretende Warnung bestätigt.

# save(stm_dtm, file = "Saved Files/stm_dtm.RData")
# DATEN LADEN: load("Saved Files/stm_dtm.RData")
used_documents <- names(stm_dtm$documents)
used_documents <- used_documents %>% gsub("^text", "", .) %>% as.integer(.)

# save(used_documents, file = "Other Files/Documents.RData")
# DATEN LADEN: load("Other Files/Documents.RData")

rm(dtm, toks)


# STM - Suche nach K ----

select_k <- searchK(stm_dtm$documents, stm_dtm$vocab, data = stm_dtm$meta,
                    K = seq(20, 120, by = 10),
                    prevalence =~ s(date) + quotecount + replycount + likecount + retweetcount,
                    init.type = "Spectral", max.em.its = 10, seed = 2021)
# ACHTUNG: EWIGE LAUFZEIT - mehrere Tage u.U., je nach Hardware - springt bei mir >16gb genutzter Arbeitsspeicher (Anzahl an Topics + Anzahl an Tweets), es ist also fraglich, ob das in dieser Art auf Maschinen mit unter 32gb RAM überhaupt läuft ...

# save(select_k, file = "Saved Files/selectK.RData")
# DATEN LADEN: load("Saved Files/selectK.RData")
plot(select_k)

selectk_df <- data.frame(K = unlist(select_k$results$K), exclus = unlist(select_k$results$exclus),
                         semcoh = unlist(select_k$results$semcoh), heldout = unlist(select_k$results$heldout),
                         residual = unlist(select_k$results$residual), bound = unlist(select_k$results$bound),
                         lbound = unlist(select_k$results$lbound), em.its = unlist(select_k$results$em.its))

selectk_df %>% select(-c(em.its, bound, residual)) %>% pivot_longer(-K, names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = K, y = value, group = measure, color = measure)) +
  geom_line() + facet_wrap(.~measure, scale = "free", ncol = 2) +
  labs(y = "Veränderung zu K-10") + scale_x_continuous(breaks = seq(20, 150, 10)) +
  theme_minimal() + theme(legend.position="none")

k_diff <- data.frame(K = selectk_df$K[2:11], Iterationen = selectk_df$em.its[2:11])
for(i in 1:10){
  k_diff$Exklusivität[i] <- selectk_df$exclus[i+1] - selectk_df$exclus[i]
  k_diff$Kohärenz[i] <- selectk_df$semcoh[i+1] - selectk_df$semcoh[i]
  k_diff$Heldout[i] <- selectk_df$heldout[i+1] - selectk_df$heldout[i]
  k_diff$LowerBound[i] <- selectk_df$lbound[i+1] - selectk_df$lbound[i]
}

k_diff %>% select(-c("Iterationen")) %>% pivot_longer(-K, names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = K, y = value, group = measure, color = measure)) + geom_hline(yintercept = 0) +
  geom_line() + facet_wrap(.~measure, scale = "free", nrow = 1) +
  labs(y = "Veränderung zu K-10") + scale_x_continuous(breaks = seq(20, 150, 10)) +
  theme_minimal() +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.1))
# Exklusivität und Heldout-Likelihood relativ stabil ab 90 Topics, Lowerbound-Verlust minimal ab 90, Kohärenzverluste minimal bei 40, 100, 120 Topics -> 90 Topics als beste Wahl. 
rm(k_diff, select_k, selectk_df, i)



# STM - Interpretation ----

stm_model_90 <- stm(stm_dtm$documents, stm_dtm$vocab, data = stm_dtm$meta,
                     K = 90,
                     prevalence =~ s(date) + quotecount + replycount + likecount + retweetcount,
                     init.type = "Spectral", max.em.its = 75, seed = 2021)
# save(stm_model_90, file = "Saved Files/stm_mod_90.RData")
# DATEN LADEN: load("Saved Files/stm_mod_90.RData")

plot(stm_model_90, type = "summary", xlim = c(0, 0.2), n = 5)
# Aufgrund der großen Anzahl an Topics ist auch dies ein Plot, der vermutlich nur als abgespeicherte Datei betrachtet werden kann. Abmessungen von 8 x 18 in werden empfohlen.
# -> Deutliche Dominanz von wenigen (Nachrichten?-)Topics, weite Verteilung der restlichen Topics und Inhalte

# Manuelle Kodierung der Topics basierend auf zentralen Worten (prob ≙ Wahrscheinlichkeit und frex ≙ Exklusivität zu Topic) sowie Top-Tweets des jeweiligen Topics, um Kategorisierung vornehmen zu können.
  

labels <- labelTopics(stm_model_90, topics = 90, n = 10)
prob <- list()
frex <- list()
mean <- list()
for(i in c(1:90)){
  prob[[i]] <- paste(labels$prob[i,], collapse = ' ')
  frex[[i]] <- paste(labels$frex[i,], collapse = ' ')
  mean[[i]] <- mean(stm_model_90$theta[, i])
}
labels_df <- data.frame(Prob = unlist(prob), Frex = unlist(frex), mean = unlist(mean), Topics = 1:90)
rm(labels, prob, frex, mean, i)

# Kodierungsregeln:
# - 3 Hauptkategorien: News, Person, Spam
#   -> News: Sachlich formulierte Sätze zu aktuellem Geschehen, wie sie sich bei Tweets von Nachrichtenorganisationen zu neuen Themen finden könnten. Das heißt nicht, dass alle diese Tweets tatsächlich von diesen Organisationen kommen, nur, dass keine Wertung aus dem Tweet klar ersichtlich wird.
#   -> Spam: Tweets, die "normale Menschen" vermutlich nicht posten würden: Entweder, weil der Tweet selbst durch übermäßiges Taggen anderer Nutzer auffällt, eine bedeutende Menge an Emoji beinhaltet (wobei hier der genaue Umbruchpunkt rein subjektiv ist und in meinen Augen  zwischen 4-5 liegt) oder ähnliches Verhalten an den Tag legt. Auf Topic-Ebene: Tweets, die zwar individuell "normal" erscheinen, sich aber über mehrere Tweets hinweg deutlich in Formulierungen und Satzstrukturen ähneln, sodass von einer gemeinsamen Quelle und/oder spezifischem Ziel ausgegangen werden kann.
#   -> Person: Tweets über private Angelegenheiten, Zitate, Nachrichtenvermittlung mit wertender Einordnung; Tweets wie sie ein "normaler Mensch" schreiben könnte.
# - Zusätzlich zu dieser groben Einteilung werden auch die jeweils behandelten Themenkomplexe (z.B. Sportnachrichten, Spam-Werbung für ein bestimmtes Produkt, Persönliche Tweets zu Workoutroutinen, ...) anhand der Top-Tweets und häufigsten/exklusivsten Worte erfasst und aufgelistet, um deren Anteil und Verteilung untersuchen zu können.
# - Auch bestimmte Worte, die sich durch die Tweets ziehen, werden festgehalten, da diese die einzelnen Topics erklären können. Sie werden mit Anführungszeichen als vorkommende Worte im Gegensatz zu abgeleiteten Überschriften markiert.

# Um einer bestimmten Hauptkategorie zugeordnet zu werden, müssen mindestens 6 der top 20 Tweets des jeweiligen Topics der Kategorie entstammen.
# Um ein Wort zugeordnet zu bekommen, muss es entweder in mindestens 6 der top 20 Tweets oder in den Prob-/Frex-Listen des jeweiligen Topics vorkommen.
# -> 6/20, damit die Kodierung die Möglichkeit zulässt, bei Topics ohne klaren Fokus alle drei Labels anbringen zu können, ohne die Kodierregeln zu brechen. 

#In Fällen, bei denen die Zuordnung knapp an dieser Grenze scheiterte, oder bei denen eine genauere Betrachtung vonnöten war, um die Inhalte einzuordnen  wurden die top 30 Tweets betrachtet und mit 9 Tweets als Schwellenwert gearbeitet.

top <- 46 #Zu betrachtendes Topic
{
  print(labels_df[top, 1])
  print(labels_df[top, 2])
  print(labels_df[top, 3])
  thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " "))
}
# Aufgrund der Länge einiger Spam-Tweets (durch das Ausschreiben der Emoji) kann es hilfreich sein, die Grafiken abzuspeichern und dann zu betrachten. Ein .PNG mit einer Hohe von 2000 Pixeln sollte dabei ausreichen.
label_csv <- read.csv("Other Files/STM_TopicLabels.csv", sep = ';', stringsAsFactors = F)
labels_df %>% mutate(content = label_csv$Label, group = label_csv$Group) %>% arrange(desc(mean)) %>% 
  select(-c(Prob, Frex)) %>% head(., 10)
# Die Topics mit dem größten erwarteten Anteil drehen sich um lokale Verbrechen (Top. 68, Platz 1), Sport (Top. 72, Platz 2) und Gerichte und -entscheidungen (Top. 66, Platz 3). All diese Topics wurden als "News" deklariert. Das erste nicht-"News"-Topic liegt als Kombination Person/Spam auf Platz 4 (Top. 3, "Workout"), das erste "Spam"-Topic beinhaltet Verschwörungstheorien zu einem scheinbar unsicheren ukrainischen Nuklear-Reaktor.
rm(thought, top, labels_df)


### Topic-Korrelation

corr <- cor(stm_model_90$theta[,1:90])
dissim <- 1 - corr
dist_mat <- as.dist(dissim)
colnames(corr) <- paste(label_csv$Number, label_csv$Label, sep = ". ")
rownames(corr) <- paste(label_csv$Number, label_csv$Label, sep = ". ")
diag(corr) <- 0
min(corr)
max(corr)
corrplot(corr, order = "hclust", hclust.method = "complete",
         cl.lim = c(-.33,.33), is.corr=FALSE, diag = F,
         insig = "blank", method="color",
         addrect = 10, cl.pos = "b", tl.cex = .45,
         tl.col = "black",rect.col = "black",
         #col = rev(colorRamp("blue2red")))
         col = c(rev(gray.colors(120))[1:43], "white", rev(gray.colors(120))[78:120]))
# Für bessere Betrachtung: Abspeicherung als .pdf (10" x 10") wird empfohlen.
# Kaum wirkliche Korrelationen, nur Antwort-Topic korrelliert mit so ziemlich jedem anderen Topic
rm(dissim, dist_mat, corr)


### Topic-Verteilungen

#Wöchentlich gemittelt
dates <- paste(year(stm_dtm$meta$date), "-", isoweek(stm_dtm$meta$date), sep = "")
for (i in 1:length(dates)) {
  if (nchar(dates[i]) == 6) {
    stri_sub(dates[i], 6, 5) <- 0
  }
} # Vereinheitlichung, "2016-8" zu "2016-08"
topic_times <- data.frame(dates, stm_model_90$theta)
counts <- topic_times %>% count(dates)
colnames(topic_times) <- c("date", paste("topic_",1:90, sep = ""))
topic_times <- aggregate(.~date, FUN = mean, data = topic_times)
topic_times_added <- topic_times # Topic mit jeweils vorherigen Topics aufsummiert für einfacheres Plotting
for(r in 1:nrow(topic_times_added)){
  for(c in 3:91){
    topic_times_added[r, c] <- topic_times_added[r, c] + topic_times_added[r, c-1]
  }
}
topic_times <- topic_times %>% mutate(count = counts[,2])
topic_times_added <- topic_times_added %>% mutate(count = counts[,2])

topic_times.long <- reshape2::melt(topic_times, id.vars = c("date", "count"))
ggplot(topic_times.long, aes(x = date, y = value * count, group = variable, color = variable)) +
  geom_line() + geom_line(aes(x = date, y = count), color = "black") +
  scale_y_sqrt() + theme_minimal() +
  labs(y = "Anzahl an Tweets", x = "Kalenderwoche") +
  scale_x_discrete(breaks = topic_times$date[seq(1, length(topic_times.long$date), by = 3)]) +
  theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = "none")
# Export als .png, 1227x900 Pixel, einmal mit Legende, und einmal ohne -> Farben extrahiert für eigens erstellte Legende, sowie Zurechtschieben Kalenderwochen

topic_times_added.long <- reshape2::melt(topic_times_added, id.vars = c("date", "count"))
ggplot(topic_times_added.long, aes(x = date, y = value * count, group = variable, color = variable)) +
  geom_line() +
  scale_y_sqrt() + theme_minimal() +
  labs(y = "Anzahl an Tweets", x = "Kalenderwoche") +
  scale_x_discrete(breaks = topic_times$date[seq(1, length(topic_times.long$date), by = 1)]) +
  theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = "none")

# Maximale Themen je Woche
max_topics <- data.frame(date = topic_times$date, topic = "")
for(i in 1:nrow(max_topics)){
  max_topics$topic[i] <- as.character(which(topic_times[i, 2:91] == max(topic_times[i, 2:91])))
}
sort(table(max_topics$topic), decreasing = T) # Mehrere Topics mit konstant dominanten Tweet-Mengen
sort(table(max_topics$topic[1:125]), decreasing = T) # Topic 3 bis Ende 2014 (#125 = KW 52 2014) dominant
sort(table(max_topics$topic[126:262]), decreasing = T) # Topic 68 bis Mitte 2017 (#262 = KW 31 2017) dominant
sort(table(max_topics$topic[43]), decreasing = T) # Topic 17 ab Mitte 2017 dominant

# Gruppiert nach Themenkomplex
topic_grp <- topic_times %>% select(-c(date, count))
topic_grp <- as.data.frame(t(topic_grp))
topic_grp <- topic_grp %>% mutate(group = label_csv$Group)
topic_grp <- aggregate(.~group, FUN = sum, data = topic_grp)
grp_names <- topic_grp[,1]
topic_grp <- select(topic_grp,-c(group))
topic_grp <- as.data.frame(t(topic_grp))
topic_grp <- topic_grp %>% mutate(date = topic_times$date, count = topic_times$count)
colnames(topic_grp) <- c(grp_names, "date", "count")

topic_grp.long <- reshape2::melt(topic_grp, id.vars = c("date", "count"))
ggplot(topic_grp.long, aes(x = date, y = value, group = variable, color = variable)) + 
  geom_line() +  labs(y = "Anteil", x = "Kalenderwoche") +
  theme(axis.text.x = element_text(angle = 90)) + theme_minimal()

ggplot(topic_grp.long, aes(x = date, y = value * 100, fill = variable)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() + 
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(breaks = topic_times$date[seq(1, length(topic_times.long$date), by = 4)]) +
  labs(y = "Anteil in %", x = "Kalenderwoche", fill = "Themengruppe") +
  theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = "bottom")
# AUch hier wieder manuelles Verschieben der X-Achsen-Labels notwendig

rm(counts, topic_grp, topic_grp.long, topic_times, topic_times.long, topic_times_added, topic_times_added.long,
   c, r, i, grp_names)



# Rückbezug auf Originaltweets ----

# Cleaning, um nur tatsächlich genutzte Dokumente zu analysieren
tweets_stm <- tweets_eng[used_documents, ]
length(unique(tweets_stm$userid))
length(unique(tweets_clean$userid))

tweets_stm <- tweets_stm %>% 
  select(c(tweetid, userid, tweet_text, tweet_time, in_reply_to_tweetid, in_reply_to_userid, quoted_tweet_tweetid, 
           quote_count, reply_count, like_count, retweet_count, hashtags, urls, user_mentions)) %>%
  mutate(max.topic = 0, topic_grp = "", max_prop = 0, colour_scale = 0)

for(i in 1:nrow(tweets_stm)){
  tweets_stm[i, 15] <- which(stm_model_90$theta[i, ] == max(stm_model_90$theta[i, ]))
  tweets_stm[i, 16] <- label_csv[unlist(tweets_stm[i, 15]), 2]
  tweets_stm[i, 17] <- max(stm_model_90$theta[i, ])
} # Zuordnung der Topics zu Tweets nach jw. Maximal-Theta des Tweet-Dokuments laut STM-Modell - Lange Laufzeit
# write_csv(tweets_stm, file.path("Other Files/tweets_stm_safetybackup.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double") Just in case

topic_prop <- data.frame(topic = 1:90, sum = 0)
for(i in 1:nrow(topic_prop)){
  test <- tweets_stm %>% filter(max.topic == i)
  topic_prop$sum[i] <- nrow(test)
}
# Selbes Muster, aber anders herum -> Tabelle über Mengenverteilung der einzelnen Topics

for(i in 1:nrow(tweets_stm)){
  tweets_stm$colour_scale[i] <- topic_prop$sum[topic_prop$topic == tweets_stm$max.topic[i]]
} # Menge der Tweets je Topic, um grafische Darstellung zu ermöglichen
ggplot(tweets_stm, aes(x = max.topic, y = max_prop, group = max.topic, fill = colour_scale)) + geom_boxplot() +
  labs(title = "Maximale theta-Werte aller Tweets, gruppiert nach zugeordnetem Topic", 
       x = "Topic", y = "theta-Wert") + scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + theme_minimal() + 
  scale_fill_continuous(trans = "sqrt",labels = scales::number, breaks = c(1000, 10000, 50000, 100000),
                        name = "Anzahl\nan Tweets\nje Topic", low = "dark red", high = "white")
# Auch wenn alle Topics Tweets mit thetha-Werten über das gesamte Spektrum beinhalten, finden sich doch einige Topics mit deutlich höheren Werten - 4 der Topics haben sogar einen Median von über 0,5.
summary(tweets_stm$max_prop)
ggplot(tweets_stm, aes(x = max_prop)) + geom_histogram(colour = "black", fill = "white", bins = 200) + 
  theme_minimal() + labs(x = "max. Theta-Wert des Tweets", y = "Anzahl")
# Während ~8.000 Dokumente quasi eindeutig zugeordnet wurden, ist die Zuordnung für den Großteil der Tweets nicht wirklich eindeutig, was gegen die Erklärkraft des STM-Modells für die Gesamtheit des Datensets spricht.

# write_csv(tweets_stm, file.path("Other Files/tweets_stm.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")
# DATEIEN LADEN: tweets_stm <- read_csv("Other Files/tweets_stm.csv", col_types = cols(tweetid = col_character(), in_reply_to_tweetid = col_character(), quoted_tweet_tweetid = col_character()))



### Inhaltsanalysen ----
# Topics mit größtem Anteil
plot(stm_model_90, type = "summary", xlim = c(0, 0.2), n = 5)

# Topic 68
top_68 <- tweets_stm %>% filter(max.topic == 68)
top_68 %>% mutate(tweet_time = as.Date(tweet_time)) %>% ggplot(aes(x = tweet_time)) + 
  geom_histogram(colour = "black", fill = "white", bins = 100) + 
  scale_x_date(date_breaks = "2 months") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
# Zwei Gruppen, Wechsel August 2016
length(unique(top_68$userid))
head(sort(table(top_68$userid), decreasing = T), 15)
top_68_accts <- users$user_display_name[users$userid %in% names(head(sort(table(top_68$userid), decreasing = T), 20))]
top_68_accts
# Hauptsächlich Nachrichten-Accounts aus unterschiedlichen Regionen der USA

top_68_first <- top_68 %>% filter(tweet_time < "2016-08-01")
top_68_last <- top_68 %>% filter(tweet_time >= "2016-08-01")
# Auf ersten Blick keine inhaltlichen Unterschiede vor/nach Einbruch
usr_first <- users$user_display_name[users$userid %in% names(head(sort(table(top_68_first$userid), decreasing = T), 20))]
usr_last <- users$user_display_name[users$userid %in% names(head(sort(table(top_68_last$userid), decreasing = T), 20))]
table(usr_last %in% usr_first)

rm(top_68, top_68_first, top_68_last, usr_first, usr_last)

# Topic 72
top_72 <- tweets_stm %>% filter(max.topic == 72)
top_72 %>% mutate(tweet_time = as.Date(tweet_time)) %>% ggplot(aes(x = tweet_time)) + 
  geom_histogram(colour = "black", fill = "white", bins = 100) + 
  scale_x_date(date_breaks = "2 months") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
top_72_accts <- users$user_display_name[users$userid %in% names(head(sort(table(top_72$userid), decreasing = T), 20))]
top_72_accts
# Erneut hauptsächlich News-Accounts
table(top_72_accts %in% top_68_accts)

rm(top_72)

# Topic 66
top_66 <- tweets_stm %>% filter(max.topic == 66)
top_66 %>% mutate(tweet_time = as.Date(tweet_time)) %>% ggplot(aes(x = tweet_time)) + 
  geom_histogram(colour = "black", fill = "white", bins = 100) + 
  scale_x_date(date_breaks = "2 months") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
top_66_accts <- users$user_display_name[users$userid %in% names(head(sort(table(top_66$userid), decreasing = T), 20))]
top_66_accts
# Wieder zu großen Teilen dieselben Accounts
table(top_66_accts %in% top_68_accts); table(top_66_accts %in% top_72_accts)

rm(top_66, top_66_accts, top_68_accts, top_72_accts)

### Falschnachrichten
tweets_eng_raw <- read_csv("Twitter Data/tweets_en-norts.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))
load("Other Files/Spike_Users.RData")
# Analysen anhand STM-Toptweets auf jeweils verwendete Formulierungen/Hashtags/..., dann Durchsuchung des Original-Datensets (englisch, keine Retweets, mit Duplikaten), um Reichweite und Umfang einschätzen zu können

# Ukrainischer Nuklearreaktor
top <- 40 # Topics: 31, 40, 46
{ thought <- findThoughts(stm_model_90, n = 20, topics = top,
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
consp.ukrNPP <- tweets_eng_raw[grepl("fukushima2015|fukushimaagain|chernobyl2015|Nukraine", 
                                     tweets_eng_raw$tweet_text, ignore.case = T), ]
consp.ukrNPP <- consp.ukrNPP %>% mutate(conspiracy = "3 NPP Ukraine\nJan. 2015", 
                                        interaction = reply_count + retweet_count + like_count + quote_count,
                                        ident_spike = userid %in% spike_usr)
summary(consp.ukrNPP$interaction); length(unique(consp.ukrNPP$userid))
sum(grepl("fukushima2015|fukushimaagain|chernobyl2015|Nukraine", tweets_stm$tweet_text, ignore.case = T))


# Columbian Chemicals
top <- 33 # Topics: 24, 26, 33
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
consp.ColChem <- tweets_eng_raw[grepl("columbianchemicals|columbianchemicalsinneworleans|Louisianaexplosion",
                                      tweets_eng_raw$tweet_text, ignore.case = T), ]
consp.ColChem <- consp.ColChem %>% mutate(conspiracy = "1 Columbian Chemicals Explosion\nSep. 2014", 
                                          interaction = reply_count + retweet_count + like_count + quote_count,
                                          ident_spike = userid %in% spike_usr)
summary(consp.ColChem$interaction); length(unique(consp.ColChem$userid))
sum(grepl("columbianchemicals|columbianchemicalsinneworleans|Louisianaexplosion", tweets_stm$tweet_text, ignore.case = T))

# Ebola in den USA
top <- 51 # Topics: 33, 51
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
consp.Ebola <- tweets_eng_raw[grepl("Ebolainatlanta|YattaQuirre", tweets_eng_raw$tweet_text, ignore.case = T), ]
consp.Ebola <- consp.Ebola %>% mutate(conspiracy = "2 Ebola in Atlanta\nDez. 13/14 2014", 
                                      interaction = reply_count + retweet_count + like_count + quote_count,
                                      ident_spike = userid %in% spike_usr)
summary(consp.Ebola$interaction); length(unique(consp.Ebola$userid))
sum(grepl("Ebolainatlanta|YattaQuirre", tweets_stm$tweet_text, ignore.case = T))

# Verunreinigtes Trinkwasser
top <- 64 # Topic: 64
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
consp.Water <- tweets_eng_raw[grepl("phosphorusdisaster", tweets_eng_raw$tweet_text, ignore.case = T), ]
consp.Water <- consp.Water %>% mutate(conspiracy = "4 Polluted Water\nMar. 10 2015", 
                                      interaction = reply_count + retweet_count + like_count + quote_count,
                                      ident_spike = userid %in% spike_usr)
summary(consp.Water$interaction); length(unique(consp.Water$userid))
sum(grepl("phosphorusdisaster", tweets_stm$tweet_text, ignore.case = T))

# Vergiftete Truthäne
top <- 61 # Topic: 61
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
consp.Turkey <- tweets_eng_raw[grepl("kochfarms|foodpoisoning", tweets_eng_raw$tweet_text, ignore.case = T), ]
# Ein einzelner Tweet nur mit #Foodpoisoning ohne Bezug zu Thanksgiving
consp.Turkey <- consp.Turkey %>% filter(tweet_time >= "2015-11-26") %>% 
  mutate(conspiracy = "5 Poisoned Turkey\nNov./Dez. 2015", interaction = reply_count + retweet_count + like_count + quote_count,
         ident_spike = userid %in% spike_usr)
summary(consp.Turkey$interaction); length(unique(consp.Turkey$userid))
sum(grepl("kochfarms|foodpoisoning", tweets_stm$tweet_text, ignore.case = T))


conspiracy_df <- rbind(consp.ColChem, consp.Ebola, consp.Turkey, consp.ukrNPP, consp.Water)
conspiracy_df %>% select(tweet_time, conspiracy) %>% ggplot(aes(x = tweet_time, fill = conspiracy)) +
  geom_histogram(bins = 30, colour = "black") + facet_wrap(.~conspiracy, scale = "free", ncol = 3) + 
  theme_minimal() +labs(x = "Tweet-Uhrzeit", y = "Anzahl an Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5), legend.position = "none")

conspiracy_df %>% group_by(conspiracy) %>% summarise(ident_spike = sum(ident_spike))
# Die erste dieser Fakenews-Kampagnen wurde von den identifizierten Accounts mit geringem englischsprachigen Anteil zumindest zu Teilen mitgetragen
length(unique(consp.ColChem$userid)); sum(unique(consp.ColChem$userid) %in% spike_usr)
# Beinahe die Hälfte der Accounts kommen aus der identifizierten Nutzergruppe mit kaum englischsprachigen Tweets
rm(consp.ColChem, consp.Ebola, consp.Turkey, consp.ukrNPP, consp.Water, conspiracy_df, 
   top, thought, spike_usr)


### Gesellschaft und Politik
# Texit
top <- 6 # Topic: 6
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.Texit <- tweets_stm[grepl("texit", tweets_stm$tweet_text, ignore.case = T), ]
# Ein News-Tweet aus 2017 hat nichts mit dem Rest zu tun
soc.Texit <- soc.Texit %>% filter(tweet_time <= "2017-01-01") %>%
  mutate(interaction = reply_count + retweet_count + like_count + quote_count)
summary(soc.Texit$interaction)

# Polizeigewalt
top <- 81 # Topics: 78, 81
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.PolBrut <- tweets_stm[grepl("policeabuse|policebrutality|policestate|baltimorevsracism|copswillbecops",
                                tweets_stm$tweet_text, ignore.case = T), ]
# Hashtags statt Tweet_text, da "acab" Johnny Depps Chupacabra mit abgreift
soc.PolBrut <- soc.PolBrut %>% mutate(interaction = reply_count + retweet_count + like_count + quote_count)

ggplot(soc.PolBrut, aes(x = tweet_time)) + geom_histogram(bins = 100)
summary(soc.PolBrut$interaction)
ggplot(soc.PolBrut, aes(x = tweet_time, y = interaction)) + geom_point() + geom_smooth()
summary(soc.PolBrut$interaction[soc.PolBrut$tweet_time < "2015-05-01"])

# Black Lives Matter
top <- 78 # Topics: 15, 78
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.blm <- tweets_stm[grepl("blacklivesmatter|blacktwitter",
                                tweets_stm$tweet_text, ignore.case = T), ] 

# Black Representation

# Clinton-Emails
top <- 55 # Topic: 55
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.Clinton <- tweets_stm[grepl("clinton",tweets_stm$tweet_text, ignore.case = T) &
                            grepl("mail",tweets_stm$tweet_text, ignore.case = T), ]

# Ehe für alle
top <- 55 # Topic: 55
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.Clinton <- tweets_stm[grepl("clinton.*mail|mail.*clinton|podesta.*mail|mail.*podesta",
                                tweets_stm$tweet_text, ignore.case = T), ]

# Wikileaks
top <- 55 # Topic: 55
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.Wiki <- tweets_stm[grepl("wikileaks|vault.*7",tweets_stm$tweet_text, ignore.case = T), ]

# Seth Rich
top <- 5 # Topic: 55
{ thought <- findThoughts(stm_model_90, n = 20, topics = top, 
                          text = tweets_eng$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " ")) }
soc.SethRich <- tweets_stm[grepl("seth rich|sethrich|dnc staffer",tweets_stm$tweet_text, ignore.case = T), ]

# Pizzagate und QAnon generell
soc.Conspir <- tweets_stm[grepl("pizzagate|qanon|podesta.*art|art.*podesta",tweets_stm$tweet_text, ignore.case = T), ]










### Strukturanalysen ----

# Interaktionen und Tweet-Mengen nach Topics
tweets_stm <- tweets_stm %>% mutate(interactions = retweet_count + like_count + quote_count + reply_count) %>% 
  mutate(inter.grp = ifelse(interactions > 0, "c 1-100 Interaktionen", "d Keine Interaktionen")) %>% 
  mutate(inter.grp = ifelse(interactions > 100, "b 101-1000 Interaktionen", inter.grp)) %>%
  mutate(inter.grp = ifelse(interactions > 1000, "a >1000 Interaktionen", inter.grp))
for.plot <- tweets_stm %>% select(c(max.topic, topic_grp, inter.grp, interactions)) %>% 
  mutate(max.topic = as.integer(max.topic)) %>%
  mutate(topic_grp = ifelse(topic_grp %in% c("News", "Person", "Spam"), topic_grp, "Undefiniert (mehrere)"))

for.plot %>% ggplot(aes(x = max.topic, fill = inter.grp)) + geom_bar(size = 1) + theme_minimal() + 
  scale_y_continuous(trans = "sqrt", labels = scales::number, breaks = c(0, 2500, 25000, 75000, 175000, 300000)) + 
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 89)) +
  scale_fill_brewer(palette = "Set1", name = "Summe aller\nInteraktionen\nje Tweet", 
                    labels = c(">1000", "101-1000", "1-100", "0")) +
  labs(x = "Topic-Nummer", y = "Anzahl an Tweets je Topic") + facet_wrap(~ topic_grp, nrow = 4)
# Wie zu erwarten, dominieren News-Topics in der Menge - aber auch in der Anzahl an Tweets. Wie es scheint, waren die meisten Tweets entweder zu News-Themen oder wurden per stm diesem Komplex zugeordnet - Personen-, Spam- und nicht zuordnbare Tweets finden sich deutlich seltener in den Daten. Zusätzlich zeigt sich eine Art Viralität der Tweets: Sie erhalten in etwa zu gleichen Teilen gar keine und nur wenig Aufmerksamkeit, oder zu gleichen Teilen mittel und viel Aufmerksamkeit.
news_top68 <- tweets_stm %>% filter(max.topic == 68) %>% filter(interactions > 0) %>% arrange(desc(interactions))
news_top68$tweet_text[sample(1:nrow(news_top68), 50)]
head(news_top68$tweet_text, 50)
# Dieses Topic umfasst dominant Berichte über krimineller Handlungen. Interessanterweise lassen sich zwei generelle Anätze in der Berichterstattung erkennen: Anti-Islam/Anti-Flüchtling/Pro-Trump-Tweets und Anti-Cop/Pro-BlackLivesMatter-Tweets.

# Zusäzlich lässt sich festhalten, dass die meisten Tweets keine einzige Interaktion aufweisen, und nur eine sehr kleine Minderheit mehr als 100 Interaktionen ansammeln konnte.
for.plot %>% filter(interactions > 500) %>% ggplot(aes(x = max.topic, y = interactions, group = max.topic)) + geom_boxplot() + 
  #geom_hline(yintercept = 100000) + 
  scale_y_continuous(trans = "log", labels = scales::number, breaks = c(600, 2000, 6000, 20000, 60000, 200000)) + 
  theme_minimal() + facet_wrap(~ topic_grp, nrow = 4) +
  scale_x_continuous(breaks = c(1, 5, 10 ,15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)) +
  labs(title = "Anzahl an Tweet-Interaktionen je Topic", x = "Topic-Nummer", y = "Interaktionen je Tweet")
# Das bedeutet jedoch nicht, dass die Topics sich strukturell in ihren Reichweite-Potenzialen unterscheiden. Ein sehr großer Anteil an Topics aus allen Bereichen weist Tweets mit fünf- oder mehrstelligen Interaktionen auf.

# "News"-Topics mit >100k-Interaktionen-Tweets:
tweets_stm %>% filter(max.topic == 39) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 57) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)

# "Undefiniert"-Topics mit >100k-Interaktionen-Tweets:
tweets_stm %>% filter(max.topic == 15) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 23) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 75) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
# Während die reichweitenstärksten Tweets des ersten der vier Topics eindeutig die Errungenschaften und Geschichten weiblicher PoC feiert, zeigt sich das zweite Topic weniger eindeutig. So finden sich neben zwei Tweets zu afrikanischen Sportlern und Doktoren dominant Tweets zu rechten Themen und Personen wie Ted Cruz und Laura Loomer - Auch wenn das Gesamtbild hier weniger eindeutig ist, als im ersten Topic. Das dritte Topic ist ebenfalls gemischt - obwohl ein dominanter Teil der Tweets sich mit Themen des Trump-Universums auseinandersetzt, finden sich drei wortgleiche Tweets, die Trump kritisieren udn angreifen. Topic vier beschäftigt sich erneut hauptsächlich mit schwarzen Amerikanern - wobei hier keine klare Rechts-Links-Zuordnung möglich ist, da sich Tweets für beide Seiten finden.

# "Person"-Topics mit >100k-Interaktionen-Tweets:
tweets_stm %>% filter(max.topic == 29) %>% filter(interactions > 0) %>%
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 36) %>% filter(interactions > 0) %>%
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 57) %>% filter(interactions > 0) %>%
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 59) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text) 
# Während die Top-Posts des vierten untersuchten Personen-Topics hauptsächlich in die Kategorie "Feelgood-Posts" zu gehören scheinen (auch wenn hier festzuhalten ist, dass ein großer Teil der Postings sich auf schwarze Kinder zu beziehen scheint), weisen die anderen drei Topics erneut politische Themen auf. Das erste untersuchte Topic beschäftigt sich dabei                                                                                   . findet sich im ersten betrachteten Topic ein ähnliches Bild wie in Topic 15, nur mit einem stärkeren Bezug auf aktuelle, negative Ereignisse wie den Tod Trayvon Martins oder die Feuerung Colin Kapernicks

# Interaktionen mit anderen Tweets
replies_stm <- tweets_stm %>% filter(!(is.na(in_reply_to_tweetid)))

for.plot <- replies_stm %>% select(c(max.topic, topic_grp, inter.grp, interactions)) %>% mutate(max.topic = as.integer(max.topic)) %>%
  mutate(topic_grp = ifelse(topic_grp %in% c("News", "Person", "Spam"), topic_grp, "Undefiniert (mehrere)"))
for.plot %>% ggplot(aes(x = max.topic, fill = inter.grp)) + geom_bar(size = 1) + theme_minimal() + 
  scale_y_continuous(labels = scales::number, breaks = c(0, 1000, 2000, 3000, 4000, 5000)) + 
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 89, 99)) +
  scale_fill_brewer(palette = "Set1", name = "Summe erhalte-\nner Interaktionen\nje Antwort", 
                    labels = c(">1000", ">100-1000", "1-100", "0")) +
  labs(x = "Topic-Nummer", y = "Anzahl an Antwort-Tweets") + facet_wrap(~ topic_grp, nrow = 4)
# Klare Dominanz von Antworten bei Spam-Tweets, mit deutlichen Unterschieden je nach Topic.

# Interne Antworten
replies_stm_int <- replies_stm %>% filter(in_reply_to_tweetid %in% tweets_stm$tweetid) %>%
  select(tweetid, userid, tweet_text, tweet_time, in_reply_to_tweetid, max.topic, topic_grp, interactions) %>% mutate(src_topic = 99, src_grp = NA)

for(i in 1:nrow(replies_stm_int)){
  r <- which(tweets_stm$tweetid == replies_stm_int$in_reply_to_tweetid[i])
  replies_stm_int$src_topic[i] <- tweets_stm$max.topic[r]
  replies_stm_int$src_grp[i] <- tweets_stm$topic_grp[r]
}

replies_stm_int %>% 
  mutate(topic_grp = ifelse(topic_grp %in% c("News", "Person", "Spam"), topic_grp, "Undefiniert (mehrere)")) %>% 
  mutate(src_grp = ifelse(src_grp %in% c("News", "Person", "Spam"), src_grp, "Undefiniert (mehrere)")) %>% 
  ggplot(aes(x = max.topic, y = src_grp)) + geom_count(aes(color = ..n.., size = ..n..)) + facet_wrap(~ topic_grp, nrow = 4) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 89, 99)) + theme_minimal() +
  labs(x = "Topic-Nummer", y = "Ziel-Topicgruppe") + facet_wrap(~ topic_grp, nrow = 4) + guides(color = 'legend')
  
# Interne Antworten gehen dominant an News-Topics - aber an welche?
replies_stm_int %>% mutate(src_grp = ifelse(src_grp %in% c("News", "Person", "Spam"), src_grp, "Undefiniert (mehrere)")) %>% 
  ggplot(aes(x = src_topic)) + geom_bar(size = 1) + theme_minimal() + 
  scale_y_continuous(labels = scales::number) + 
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 89, 99)) +
  scale_fill_discrete(name = "Summe aller\nInteraktionen\nje Tweet", labels = c(">1000", ">100-1000", "1-100", "0")) +
  labs(title = "Zieltopics interner Antworten", x = "Topic-Nummer", y = "Antworten auf jw. Topic") + facet_wrap(~ src_grp, nrow = 4)
# Das Topic mit den meisten Tweets dominiert mit den meisten erhaltenen Antworten. Die Topics #70 (Spam) und #89 (News) erhielten aber deutlich mehr Antworten, als sie anteilsmäßig an Tweets aufweisen.
tweets_stm %>% filter(max.topic == 69) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 70) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text)
tweets_stm %>% filter(max.topic == 89) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions)) %>% select(tweet_text) %>% head(n = 15) %>% pull(tweet_text) 
# Dominant anti-Flüchtlings- und anti-Islam-Rhetorik.




### Analyse von News-Topics mit gesellschaftlicher Relevanz: Überprüfung aller News-Topics auf mehrfach auftretende Themenkomplexe zu politischen/gesellschaftlihen Themen, die sich in ein Rechts-Links-Spektrum einordnen lassen und Interaktionen erhielten.
# Aufbau Rechts-Links-Spektrum durch Extraktion von Kern-Themen und -Phrasen aus den jeweiligen Topics und Zuordnung zu jeweiliger Seite.
news_top <- tweets_stm %>% filter(max.topic == 4) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions))
head(news_top$tweet_text, 50)
news_top$tweet_text[sample(1:nrow(news_top), 50)]
# Tweets zu einer Reihe an rechten und linken Märschen und Demonstrationen.
rw_news <- tweets_stm[tweets_stm$tweetid %in% news_top$tweetid[
  which(grepl("sharia|marchforlife|renamemillion|libs", 
              news_top$tweet_text, ignore.case = T))], c(1:4, 15, 19)]
lw_news <- tweets_stm[tweets_stm$tweetid %in% news_top$tweetid[
  which(grepl("blackhistory|black", 
              news_top$tweet_text, ignore.case = T))], c(1:4, 15, 19)]
lw_news <- lw_news[!(grepl("makeamovieblack", lw_news$tweet_text, ignore.case = T)), ] #Entfernen eines aufgegriffenen Hashtags


news_top <- tweets_stm %>% filter(max.topic == 5) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions))
head(news_top$tweet_text, 50)
news_top$tweet_text[sample(1:nrow(news_top), 50)]
# Angriffe auf demokratische Abgeordnete, rechte Verschwörungstheorien um Wikileaks-Releases und Seth Rich -> Anti-Hillary.
table(grepl("seth rich|sethrich|pizzagate", news_top$tweet_text, ignore.case = T))
# Obwohl nur wenige Tweets Seth Rich erwähnen, und sich nur je ein Tweet zu Pizzagate und gegen Uranium One finden, tauchen alle diese Themen in den Top-Tweets des Topics auf.
rw_news <- bind_rows(rw_news, tweets_stm[tweets_stm$tweetid %in% news_top$tweetid[
  which(grepl("seth rich|sethrich|pizzagate", 
              news_top$tweet_text, ignore.case = T))], c(1:4, 15, 19)])


news_top <- tweets_stm %>% filter(max.topic == 8) %>% filter(interactions > 0) %>% 
  arrange(desc(interactions))
head(news_top$tweet_text, 50)
news_top$tweet_text[sample(1:nrow(news_top), 50)]
# Pro-Trump Tweets zu diversen Topics, Zelebrierung der Errungenschaften schwarzer Amerikaner und Hinweise auf verstorbene schwarze Amerikaner -> Polizeigewalt?
table(grepl("migrant|build the wall|build that wall|guccifer|seth rich", news_top$tweet_text, ignore.case = T))
# Obwohl nur wenige Tweets Seth Rich erwähnen, und sich nur je ein Tweet zu Pizzagate und gegen Uranium One finden, tauchen alle diese Themen in den Top-Tweets des Topics auf.
rw_news <- bind_rows(rw_news, tweets_stm[tweets_stm$tweetid %in% news_top$tweetid[
  which(grepl("migrant|build the wall|build that wall", 
              news_top$tweet_text, ignore.case = T))], c(1:4, 15, 19)])










news_top68 <- tweets_stm %>% filter(max.topic == 68) %>% filter(interactions > 0) %>% arrange(desc(interactions))
news_top68$tweet_text[sample(1:nrow(news_top68), 50)]
head(news_top68$tweet_text, 50)
# Während sich das Topic aus allen möglichen Nachrichtenmeldungen zu Kriminalität und Unfällen besteht, finden sich in den Top-Tweets zwei  gegensätzliche Strömungen: Zum Einen Pro-Polizei-Berichte zu muslimischen Tätern und terroristischen Anschlägen, insbesondere in Europa und zum Anderen Anti-Polizei-Berichte zu übermäßigem Gewalteinsatz von US-Polizisten insbesondere gegen schwarze Bürger. Es zeigt sich also bereits hier der Ansatz einer Aufspaltung in rechte/linke Echokammern.
news_top66 <- tweets_stm %>% filter(max.topic == 66) %>% filter(interactions > 0) %>% arrange(desc(interactions))
news_top66$tweet_text[sample(1:nrow(news_top66), 50)]
head(news_top66$tweet_text, 50)


news_top8 <- tweets_stm %>% filter(max.topic == 8) %>% filter(interactions > 0) %>% arrange(desc(interactions))
news_top8$tweet_text[sample(1:nrow(news_top8), 50)]
head(news_top8$tweet_text, 50)
# WIe bereits zuvor, Großteil an Tweets scheinn unzusammenhängende Nachrichten zu Ereignisssen und bekannten Personen zu sein, aber die Top-Ergebnisse zeigen zu großen Teilen eine zelebrierung der Errungenschaften und Opfer schwarzer.







table(grepl("sharia", news_top66$tweet_text, ignore.case = T))


news_top68_subL <- news_top68[grepl("black", news_top68$tweet_text, ignore.case = T),]
news_top68_subR <- news_top68[grepl("muslim|refugee|paris|manchester|europe", news_top68$tweet_text, ignore.case = T),]
table(unique(news_top68_subL$userid) %in% news_top68_subR$userid)
table(unique(news_top68_subR$userid) %in% news_top68_subL$userid)
# Etwa die Hälfte der Accounts mit "linken" Themen bedienen auch "rechte" Themen, während fast alle Accounts mit "rechten" Themen auch "linke" Themen bedienen.

# Interaktionen mit diesen Tweets



# Max.Theta-Filter (EXPERIMENTELL)
tweets_stm <- tweets_stm %>% mutate(max.topic = ifelse(max_prop >= 0.15, max.topic, 99)) %>%
  mutate(topic_grp = ifelse(max.topic == 99, "undef", topic_grp))


tweets_stm %>% ggplot(aes(x = interactions)) + geom_histogram(bins = 70) + 
  scale_x_continuous(trans = "sqrt") + scale_y_continuous(trans = "sqrt")





# Oh No Its Distances
text <- tweets_eng$tweet_text
start_time <- Sys.time()
ohboyruntime <- stringdistmatrix(text, text)
end_time <- Sys.time()

end_time - start_time