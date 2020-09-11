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
  setwd("Y:\\Twitter Bachelor")
}

### Der in dieser Datei präsentierte Code ist als konstant durchlaufendes Script gedacht - Vollständiges Markieren und Ausführen ist also möglich, wird aber aufgrund der voraussichtlichen Rechenzeit nicht angeraten. Da einige der im Folgenden erzeugen Dateien aus aufwändigen und/oder rechenintensiven Schritten entstehen, besteht die Möglichkeit, diese komplexeren Elemente direkt zu laden. Aus diesem Grund werden sich an einzelnen Punkten in auskommentierter Form die Codes zum Speichern und Laden von Workspace-Dateien der jeweils erzeugten Daten finden.
# Sollte man nach einer bestimmten Datei suchen, oder einen überblick über alle verfügbaren Workspace-Elemente haben wollen, so findet sich im zweiten R-Script ("Code - Data Loading.R") der Speicher- und Ladecode gebündelt und in übersichtlicher Form.



# Verwendete Twitter-Datensätze ----
users <- read_csv("Twitter Data/ira_users_csv_hashed.csv") # Datensatz der Nutzer, Version vom 05.02.2019 (aktuellste Version, Stand Juli 2020)

tweets <- read_csv("Twitter Data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_factor(), longitude = col_factor(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 11.02.2019 (aktuellste Version, Stand Juli 2020)
tweets_oldver <- read_csv("Twitter Data/ira_tweets_csv_hashed_alt.csv", col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(), latitude = col_character(), longitude = col_character(), poll_choices = col_character())) # Datensatz der Tweets, Version vom 15.10.2018

head(as_tibble(tweets), n=20)
# Nutzung von read_csv (readr) statt read.csv (base), da base-Funktion ohne großen Aufwand nicht zur Darstellung der unterschiedlichen Schriftsätze (westlich, kyrillisch, arabisch, ...) fähig zu sein scheint.

# !ACHTUNG!
# Es ist aufgrund der schieren Größe der Tweet-Datensätze (jeweils >5GB) je nach vorhandenem Arbeitsspeichers dazu zu raten, diese nur dann zu laden, wenn sie aktiv benötigt werden und nach Gebrauch zu entladen ( rm(NAME) ). Sollte dies den Speicher nicht komplett leeren, kann mit Garbage Collection oder R-Neustart nachgeholfen werden ( gc() bzw. .rs.restartR() respektive).



# Analyse Users: ----

### Account-Sprachen
languages <- data.frame(sort(table(users$account_language), decreasing = T))
languages
head(users$user_profile_description[users$account_language == "ar"], 10)
# Vermutlich ISO 639-1 bzw. lokalisierte (en-gb, zh-cn) Tags. Der Großteil der Accounts sind englischsprachig eingestellt, aber ein knappes Drittel gibt russisch als Sprache an. West- u. Zentraleuropäische Accounts sind die drittgrößte Gruppe (Deutschland, UK, Frankreich, Spanien, Italien) vor arabischen Accounts. Vereinzelte Chinesen, Indonesier und Ukrainer.

#Aufbereitung als Data Frame, Gruppierung und Vorbereitung für Vergleich mit angegebenen Orten
lang1 <- as.character(languages[, 1])
lang2 <- languages[, 2]
other <- which(lang1 %in% c("zh-cn", "id"))
europe <- which(lang1 %in% c("de","en-gb", "fr", "es", "it"))
lang1 <- append(lang1, c("european", "others", "fantasy", "NA")) # fantasy + NA -> Platzhalter für später
lang2 <- append(lang2, c(sum(lang2[europe]), sum(lang2[other]), 0, sum(is.na(users$account_language))))
lang1 <- lang1[-c(other, europe)]
lang2 <- lang2[-c(other, europe)]
loclang <- data.frame(lang1, lang2)
names(loclang) <- c("language", "language_n")
rm(lang1, lang2, other, europe)

loclang
sum(loclang$language_n)

### Orte
table(users$user_reported_location)
#Keine eindeutige Sortierung - wie zu vermuten bei individuellen Angaben - also manuelle Nachsortierung notwendig! (Im Folgenden hinter {Klammern} versteckbar für bessere Übersicht und einfache Code-Ausführung)
{
  users$shortened_location <- ifelse(users$user_reported_location %in% c("U.S.A", "USA", "Usa", "usa", "US", "us", "united states", "United States", "America", "AMERICA", "Amerika", "THE US", "mother America", "Murica", "Estados Unidos", "Douglas, United States"), "US", NA)
  # US-Bundesstaaten und D.C.
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Troy, Alabama", "alabama", "AL"), "US, Alabama", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Alaska"), "US, Alaska", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Arizona, USA", "Arizona", "arizona", "ARIZONA", "City of Phoenix, Arizona", "Phoenix", "Mesa", "AZ"), "US, Arizona", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("arkansas"), "US, Arkansas", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Venice, California", "Redwood City, California", "San-Francisco", "Oakland, CA", "California", "California, USA", "CA", "UC Davis", "Santa Barbara, California", "San Francisco", "San Francisco, CA","SF", "San Diego", "San DIego, CA", "San Diego, United States", "Rancho Rinconada, CA, USA", "San Diego, CA", "Riverside", "Santa Monica", "Los Gatos, California", "Los Angeles, CA", "Los ANgeles", "los angeles", "Los-Angeles", "Los Angeles", "Hollywood", "Fowler, California"), "US, California", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("La Junta, Colorado", "Denver, CO", "Colorado"), "US, Colorado", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("New Haven"), "US, Conneticut", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Delaware, USA"), "US, Delaware", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Weeki Wachee, Florida", "USA, FL", "North Port, FL", "Destin, Florida", "Orlando", "Orlando, FL", "Tallahassee", "Miami", "Miami, FL", "Miami, USA", "jacksonville", "Jacksonville", "Florida, USA", "Florida"), "US, Florida", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("USA, Atlanta", "ATL", "ATL, GA", "Atlanta", "Atlanta, GA", "Atlanta, Georgia", "Macon, GA", "Georgia", "Georgia, USA", "Druid Hills, GA", "Downtown, Atlanta", "Brookhaven, GA"), "US, Georgia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c(), "US, Hawaii", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("ID"), "US, Idaho", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Aurora, Illinois", "Chicago", "Chicago, IL"), "US, Illinois", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("INDIANAPOLIS, USA"), "US, Indiana", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c(), "US, Iowa", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Wichita, KS", "Kansas", "Kansas, USA"), "US, Kansas", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Louisville"), "US, Kentucky", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Baton Rouge, LA", "New Orleans, LA", "New Orleans", "New Orlean", "Louisiana", "LA", "Lafayette, LA"), "US, Louisiana", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Brunswick, ME, USA"), "US, Maine", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Baltimore", "Baltimore, MD", "temple hills, md"), "US, Maryland", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Salem, Massachusetts", "Boston", "Boston, MA", "Boston, USA"), "US, Massachusetts", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Bloomfield Hills, Michigan", "Michigan", "Flint, MI", "Detroit", "Detroit, Michigan"), "US, Michigan", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("MN", "Mn", "Minnesota", "Minneapolis, MN", "Menisotta"), "US, Minnesota", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Jackson, MS", "Jackson"), "US, Mississippi", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("StLouis", "St Louis, MO", "Missouri, USA", "Kansas City, MO", "Ferguson, MO"), "US, Missouri", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Montana"), "US, Montana", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Nebraska, USA"), "US, Nebraska", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Vegas", "Nevada", "Las Vegas"), "US, Nevada", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("NH", "New Hampshire"), "US, New Hampshire", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("NJ", "Newark, NJ", "Camden, NJ", "New Jersey", "New Jersey, USA", "Old Bridge, New Jersey"), "US, New Jersey", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("NM", "Albuquerque", "Albuquerque, NM", "Eldorado at Santa Fe, NM, USA", "Los Alamos"), "US, New Mexico", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("NYC", "nyc", "NY City", "NY", "ny", "New York, USA", "New York, NY", "New York City", "New - York", "New-York", "Bronx, NY", "brooklyn", "Brooklyn", "Brooklyn, NY", "Brkln", "The Big Apple", "Queens, NY", "New York", "Manhattan, NY", "Garden City, NY", "Johnson City, New York", "East Aurora, New York", "Baldwinsville, New York", "Buffalo"), "US, New York", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("North Carolina", "Raleigh, North Carolina", "Greensboro"), "US, North Carolina", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("North Dakota"), "US, North Dakota", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Whitehall, Ohio", "Cincinnati, OH", "Cleveland, OH", "cleveland / ohio", "Cleve", "Columbus, Ohio", "Ohio", "Ohio, USA", "OH", "Montgomery", "Montgomery, Ohio", "Millville village, OH, USA", "Milford, Ohio", "Grove City, Ohio", "Green village, OH, USA", "City of Cleveland, USA", "Cincinnati"), "US, Ohio", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Oklahoma", "Oklahoma City", "Oklahoma, PA", "Oklahoma, USA"), "US, Oklahoma", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("portland", "Portland"), "US, Oregon", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Pittsburgh", "Pittsburgh, PA", "Pittsburgh, US", "Philadelhia", "Philadelphia", "Philadelphia, PA", "Philly", "Pennsylvania", "Mohnton, PA", "Chester, PA"), "US, Pennsylvania", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Rhode Island", "Rhode island"), "US, Rhode Island", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("South Carolina, USA", "Columbia, SC", "Charleston, SC"), "US, South Carolina", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c(), "US, South Dakota", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Tennessee, USA", "TN", "Nashville", "Memphis", "Memphis, TN"), "US, Tennessee", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Texas, USA", "Texas", "Austin", "Austin, TX", "Ostin", "All Over Texas", "City of San Antonio, TX", "Dallas", "Dallas, Texas", "Stonewall, TX", "Houston", "Houston, TX", "El Paso, Texas", "Dallas, TX"), "US, Texas", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Utah", "utah", "Salt Lake City"), "US, Utah", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c(), "US, Vermont", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Winchester, Virginia", "Richmond, VA"), "US, Virginia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("WA", "Washington", "Seattle, WA", "Seattle", "Stanwood city, WA, USA"), "US, Washington", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c(), "US, West Virginia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Wisconsin, USA", "Milwaukee", "Milwaukee, WI", "Madison"), "US, Wisconsin", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c(), "US, Wyoming", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Washington D.C", "Washington D.C.", "Washington, D.C.", "Washington, DC"), "US, Washington D.C.", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Guanica zona urbana, PR, USA"), "US, Unincorporated", users$shortened_location)
  # Russland
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Krasnoyarsk", "Krasnoyarsk, Russia", "Ekaterinburg, Russia", "Chelyabinsk, Russia", "Volgograd", "Velikiy Novgorod, Russia", "Stavropol, Russia", "Ufa, Russia", "Rostov-na-Donu, Russia", "russia", "Russia", "Russian Empire", "novgorod", "Tomsk", "Republic of Chechnya, Russia", "Crimea, Russia", "Perm, Russia", "Penza", "Omsk, Russia", "Nizhniy Novgorod, Russia", "Murmansk, Russia", "Irkutsk", "Chelyaba", "belgorod", "✴Новгород✴", "Уфа", "Ульяновск", "Тула, Тульская область", "уфа", "ул. Ленина", "ростов-я-тону", "россия", "омск", "новосибирск", "новгород", "екб", "днищний дновгород", "Ярославль, Россия", "Ярославль", "Чита", "Чеченская республика, Россия", "Челябинск", "Челны", "Чебоксары, Россия", "Чебоксары", "Ханты-Мансийск", "Хабаровск, Россия", "Хабаровск", "Тула, Россия", "Тула", "Тува", "Томск", "Тольятти", "Тверь", "Тверское подворье", "Ташла", "Тамань", "Сраратов", "Сочи, Россия", "Сочи", "Смоленск", "Симферополь", "Саров", "Саратов )))", "Саратов", "Саранск", "Самара", "Рязань", "Ростов-на-Дону, Россия", "Ростов-на-Дону", "Ростов-На-Дону"), "Russia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Ростов", "Россия, Казань", "Россия", "Российская Федерация", "РФ", "Пятигорск", "Пенза", "Орел", "Омск, Россия", "Омск", "Нягань", "Мурманск", "Магадан", "Люберцы", "Луга", "Липецк", "Курск", "Красноярск", "Киров", "Кемерово", "Петрозаводск", "Пермь", "овосибирск, Россия", "Новосибирск", "Новосиб", "Новокузнецк, Россия", "Новгород", "Новосибирск, Россия", "Краснодар", "Котлас", "Кострома", "Кольский п-в", "Новосибирск, Россия", "Нижний Новогород", "Нижний Новгород", "Нижний", "Екатеринбург...", "Екатеринбург", "ЕКБ", "Ё-бург", "Нижний Новгород", "Нижний", "Набережные Челны, Россия", "Калуга, Россия", "Калуга", "Казань", "Калининград, Россия", "Калининград", "Иркутск", "Иваново", "Елец", "Барнаул, Россия"), "Russia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c( "Грозный", "Грозный", "Вся Россия", "Вся Россия", "Воронеж, Россия", "Воронеж", "Вологда", "Волгоград, Россия", "Волгоград, Вася!", "Волгоград", "Волгодонск, Россия", "Великий Новгород", "Брянск", "Белгород", "Башкартостан", "Архангельск, Россия", "Архангельск"), "Russia", users$shortened_location)
  # Aufsplittung in mehrere Befehle umgeht komische Sytax-Fehler
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Russia, Moscow", "moscow city", "Moscow-City", "Moscow", "msk", "Msk", "MSK", "MSK SAO", "★москва", "Moscow, Russia", "moscow", "Москва♥", "Москоу-сити", "осква, Россия", "Москва Златоглавая", "Москва Белокаменная", "Москва (СССР - Россия)", "Москва - столица", "Москва - Самара", "Москва - Лондон", "Москва", "Моска", "мск", "москва", "Туапсе, Москва", "Серпухов", "Севастополь", "Россия, Москва", "Пушкино", "Псков", "Подольск", "Одинцово", "Москва, Россия", "МСКВА", "МСК", "Коломна", "Перово", "МSK", "Дмитров"), "Russia, Moscow", users$shortened_location) # Moskau-Stadt + Oblast
  users$shortened_location <- ifelse(users$user_reported_location %in% c("St-Petersburg", "St. Petersburg", "st.petersburg", "St.Petersburg", "st/petersburg", "Saint Petersburg, Russia", "Saint Petersburg", "saint-petersburg", "Saint-Peterburg", "saint P.", "saint p.", "Saint-P.", "Saint-P", "St.P", "St.P.", "St-P", "SPb♥○•°•☆", "SPB", "SPb", "spb", "cанкт-петербург", "спб", "Спб", "Сестрорецк, Россия", "Санкт-петербург", "Санкт-Петербург, Россия", "Россия Санкт-Петербург", "Санкт-Петербург", "СПб", "СПБГПУ", "СПБ", "С.Петербург", "С-Пб", "Пушкин, Санкт-Петербург", "Пушкин", "ПЕТЕРБУРГ", "Лос-Питербургос", "Ленинград", "Кронштадт", "Кингисепп", "Петроград", "Петербург", "Выборг"), "Russia, St.Petersburg", users$shortened_location) # St.Petersburg + Oblast Leningrad
  # Sonstige Länder
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Kiel, Schleswig-Holstein", "Köln, Deutschland.", "Hessen, Deutschland", "Hamburg, Deutschland", "Frankfurt am Main, Deutschland", "Frankfurt am Main, Hessen", "Erfurt, Deutschland", "Düsseldorf, Deutschland", "Dresden, Sachsen", "Bremen, Deutschland", "Berlin, Deutschland", "Stuttgart, Deutschland", "Rostock, Deutschland", "Saarbrücken, Deutschland", "Deutschland", "München, Bayern", "Magdeburg, Deutschland", "Köln, Deutschland", "Потсдам"), "Germany", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("UK", "Newcastle", "Newport", "Manchester", "London", "London, England", "London, UK", "Liverpool", "Coventry"), "United Kingdom", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Italy", "Italia", "italia", "Milano, Lombardia", "Itala, Sicilia"), "Italy", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Paris", "Paris, France", "Lyon"), "France", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Brussel, België"), "Belgium", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Стокгольм"), "Sweden", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Украина", "Покровское", "Одесса", "Мариуполь", "Луцк", "Луганск", "Кременчуг", "Київ", "Киев", "Запорожье", "Житомир", "Донецк, Россия", "Донецк", "Днепр, Украина", "Днепр"), "Ukraine", users$shortened_location) # Krim zu Russland, Donetsk zu Ukraine
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Таллин"), "Estonia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Прага"), "Czechia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Мінск", "Витебск", "Беларусь"), "Belarus", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Стамбул"), "Turkey", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Cairo, Egypt", "مصر"), "Egypt", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Beirut", "لبنان", "بيروت"), "Lebanon", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("العراق"), "Iraq", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("баку"), "Azerbaijan", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("syria", "Syria", "Damascus", "Aleppo", "سورية", "دمشق", "سوريا", "حماة", "حمص"), "Syria", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Oliva Pizza & Pasta // Amman", "Zarqa, Jordan", "Amman", "Az-Zarqa", "حلب", "اللاذقية", "المملكة الأردنية الهاشمية", "الأردن"), "Jordan", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("الدمام"), "Saudi-Arabia", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Манила"), "Philippines", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Manama, Bahrain"), "Bahrain", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("عمان"), "Oman", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Пхеньян"), "North Korea", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("katamandu"), "Nepal", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("ZM"), "Zambia", users$shortened_location)
  # Other
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Universe", "Wonderland", "sin city", "Love Town", "Liberty City", "Fattyland", "garage", "hood", "I AM A CITIZEN OF THE UNIVERSE", "Islamic States of America", "dreamland", "Black America", "хогвортс", "Cosmopolitanism", "2148", "الدولة الإسلامية", "мой мир", "амфетаминовая столица", "Я везде", "Черемухи", "ЦАО", "Третий Рим",  "Раша", "Работа))", "Мечтовиль", "Мечта", "Красти Бургер", "Артем", "#РусскийМир"), "Other", users$shortened_location)
  users$shortened_location <- ifelse(users$user_reported_location %in% c("Newhustle", "vladik", "Man", "piter", "Jersey", "KN", "Birmingham", "Cromwell", "ширкино", "Ch", "сжигай толерантный рай,слышишь", "сейчас орел", "Таллин, Санкт-Петербург", "Сибирь матушка", "Северная столица", "♠Питер♠", "питер", "Питер", "Новгород - СПб", "НН", "Владик ДВФУ", "Вильнюс - Москва", "Ватноград", "Бутово", "Адлер", "Adler", "☼Новосибчик☼"), "Unidentified", users$shortened_location)
}
table(users$shortened_location)
sum(is.na(users$shortened_location))
length(grep("US", users$shortened_location))
length(grep("Russia", users$shortened_location))

#Grouping
arabic <- length(grep("Bahrain", users$shortened_location)) + length(grep("Egypt", users$shortened_location)) + length(grep("Lebanon", users$shortened_location)) + length(grep("Iraq", users$shortened_location)) + length(grep("Syria", users$shortened_location)) + length(grep("Jordan", users$shortened_location)) + length(grep("Saudi-Arabia", users$shortened_location)) + length(grep("Oman", users$shortened_location))
easteuro <- length(grep("Belarus", users$shortened_location)) + length(grep("Czechia", users$shortened_location)) + length(grep("Estonia", users$shortened_location)) + length(grep("Ukraine", users$shortened_location)) + length(grep("United Kingdom", users$shortened_location))
westeuro <- length(grep("Belgium", users$shortened_location)) + length(grep("France", users$shortened_location)) + length(grep("Germany", users$shortened_location)) + length(grep("Italy", users$shortened_location)) + length(grep("Sweden", users$shortened_location))
other <- length(grep("Azerbaijan", users$shortened_location)) + length(grep("Nepal", users$shortened_location)) + length(grep("North Korea", users$shortened_location)) + length(grep("Philippines", users$shortened_location)) + length(grep("Turkey", users$shortened_location)) + length(grep("Zambia", users$shortened_location))

loc1 <- c("US", "Russia", "Arabic", "East Europe", "West Europe", "other", "Fantasy", "NA")
loc2 <- c(length(grep("US", users$shortened_location)), length(grep("Russia", users$shortened_location)), arabic, easteuro, westeuro, other, (length(grep("Unidentified", users$shortened_location)) + length(grep("Fantasy", users$shortened_location))), sum(is.na(users$shortened_location)))
loclang <- loclang %>% mutate(location = loc1, location_n = loc2)
rm(arabic, easteuro, westeuro, other, loc1, loc2)
loclang
# Konsistenz zwischen den Spracheinstellungen und Ortsangaben. Englisch (en) überwiegt bei den Sprachen zwar deutlich im Vergleich mit den angegebene Orten, aber da englisch global dominant ist, ist davon auszugehen, dass auch Nutzer in anderen Ländern ihre Accounts auf englisch einstellen (-> Europa) - oder dass es einfach die Standardeinstellung ist, und diese Nutzer sie nie geändert haben. Der deutliche Anstieg in Osteuropa lässt sich durch die Tatsache erklären, dass bis auf zwei ukrainisch-sprachige Angaben (was sich mit den languages deckt) alle Ortsangaben aus diesem Gebiet auf russisch waren.


### Account-Erstelldaten
quartals <- c(as.Date("2009-01-01"), as.Date("2009-04-01"), as.Date("2009-07-01"), as.Date("2009-10-01"))
for(i in 10:18){
  year <- as.character(2000 + i)
  for(q in c("-01-01", "-04-01", "-07-01", "-10-01")){
    quartals <- append(quartals, as.Date(paste(year, q, sep ="")))
  }
}
rm(i, q, year)
quartals <-  quartals[1:39]
hist(users$account_creation_date, breaks = quartals)
# Der Großteil der Accounts wurde im Zeitraum 2. Häfte 2013 - 1. Häfte 2014 erstellt - lange vor dem US-Wahlkampf 2016. Mögliche Erklärungen: Zeitnutzung, um Accounts als "seriös" zu etablieren, oder Nutzung der Accounts zur Beeinflussung anderer Themen als der Wahl.


### Gefolgte Accounts
hist(users$following_count)
hist(log2(users$following_count))
summary(users$following_count)
# Ein Großteil der Accounts folgt nur ein paar Hundert Accounts (wenn überhaupt), während einige wenige Accounts mehreren zehntausend Accounts folgen -> Wahrscheinlich Nutzung von Follow-Bots, um Nummern zu erhöhen.

followbots <- users[which(users$following_count >= 10000), ]
followbots <- followbots[, 7:8]
followbots
summary(lm(followbots$follower_count ~ followbots$following_count))
summary(lm(users$follower_count ~ users$following_count))
# Für die Accounts, die über 10.000 anderen Accounts folgen, besteht ein deutlicher Zusammenhang zwischen der Anzahl gefolgter Accounts und der Anzahl eigener Follower. Während für alle 3.600 Accounts die Anzahl gefolgter Accounts gut 25% der Varianz in den eigenen Follower-Zahlen erklärt (adj. R^2 = 0,246), ist es für die Accounts mit über 10.000 Follows eine Varianzaufklärung von über 60% (adj. R^2 = 0,611)!



# Analyse Tweets ----

### Tweet-Zeiten
times <- tibble(dt = tweets$tweet_time %>% ymd_hms()) %>%
  mutate(timepart = hms::hms(as.numeric(dt - floor_date(dt, "1 day"), unit="secs")), timeset = as.integer(substr(timepart,1,2)))
ggplot(times, aes(x = timeset)) + geom_bar() + theme_minimal() + ggtitle("Tweets by UTC time, all tweets")
#Es gibt deutlich erkennbare Muster in den Tweetzeiten. So werden eine große Menge Tweets zwischen 07:00 und 17:00 UTC abgesetzt, während zwischen 21:00 und 06:00 UTC bedeutend weniger Tweets verfasst wurden.
#Das bedeutet, dass die Großzahl der Tweets nach amerikanischer Sicht zwischen 03:00 und 13:00 (Ostküste) bzw. 00:00 und 10:00 (Westküste) verfasst wurden. Geht man von russischen Verfassern aus, so liegt die Hochfrequenz zwischen 10:00 und 20:00 (Moskau/St. Petersburg). Es gibt also entweder Hochzeiten während der Morgensunden in Amerika oder während den Arbeitsstunden in West-Russland.

times_eng <- tweets %>% filter(tweet_language %in% c("en")) %>% tibble(dt = tweet_time %>% ymd_hms()) %>%
  mutate(timepart = hms::hms(as.numeric(dt - floor_date(dt, "1 day"), unit="secs")), timeset = as.integer(substr(timepart,1,2)))
ggplot(times_eng, aes(x = timeset)) + geom_bar() + theme_minimal() + ggtitle("Tweets by UTC time, english language tweets")
# Filtert man nur nach englischsprachigen Tweets, so verschiebt sich das Maximum auf 13:00-17:00 UTC, mit einem Minimum zwischen 03:00-08:00 UTC.
#Somit sind die Minima bei 20:00-01:00 (Westküste) und 23:00-04:00 (Ostküste), bzw. 6:00-11:00 (Moskau) und die Maxima bei 06:00-01:00 (Westküste) und 09:00-04:00 (Ostküste), bzw. 16:00-11:00 (Moskau).


### Tweet-Sprachen nach Account

# Analyse der Anzahl englisch-/russischsprachiger Tweets für alle Accounts in den Daten, um nach sprachlicher EInheitlichkeit oder systematischen Veränderungen zu suchen. Aufgrund der großen Account-Anzahl (3608) aufgesplittet in mehrere Plots. Sollte sich im Plot-Fenster nach Ausführen des Print-Befehls kein Ergebnis zeigen, so kann es helfen, dieses zu vergrößern. Eine tatsächliche Analyse der Grafiken ist in RStudio selbst nicht möglich, die Dateien können jedoch als PDF exportiert und dann betrachtet werden - Exportmaße von ca. 40x80″ werden empfohlen.
langplot_1 <- tweets %>% select(c(userid, tweet_language, tweet_time)) %>% filter(userid %in% users$userid[1:600]) %>%
  mutate(tweet_language =  ifelse(tweet_language == "en" | tweet_language == "ru", tweet_language, "other")) %>%
  ggplot(aes(x = as.Date(tweet_time), fill = tweet_language)) + geom_histogram() +
  scale_fill_manual(values = c("en" = "red", "ru" = "blue", "other" = "green")) + 
  theme(legend.position = "none") + facet_wrap(~ userid, ncol = 30, scales = "free_y")
langplot_2 <- tweets %>% select(c(userid, tweet_language, tweet_time)) %>% filter(userid %in% users$userid[601:1200]) %>%
  mutate(tweet_language =  ifelse(tweet_language == "en" | tweet_language == "ru", tweet_language, "other")) %>%
  ggplot(aes(x = as.Date(tweet_time), fill = tweet_language)) + geom_histogram() +
  scale_fill_manual(values = c("en" = "red", "ru" = "blue", "other" = "green")) + 
  theme(legend.position = "none") + facet_wrap(~ userid, ncol = 30, scales = "free_y")
langplot_3 <- tweets %>% select(c(userid, tweet_language, tweet_time)) %>% filter(userid %in% users$userid[1201:1800]) %>%
  mutate(tweet_language =  ifelse(tweet_language == "en" | tweet_language == "ru", tweet_language, "other")) %>%
  ggplot(aes(x = as.Date(tweet_time), fill = tweet_language)) + geom_histogram() +
  scale_fill_manual(values = c("en" = "red", "ru" = "blue", "other" = "green")) + 
  theme(legend.position = "none") + facet_wrap(~ userid, ncol = 30, scales = "free_y")
langplot_4 <- tweets %>% select(c(userid, tweet_language, tweet_time)) %>% filter(userid %in% users$userid[1801:2400]) %>%
  mutate(tweet_language =  ifelse(tweet_language == "en" | tweet_language == "ru", tweet_language, "other")) %>%
  ggplot(aes(x = as.Date(tweet_time), fill = tweet_language)) + geom_histogram() +
  scale_fill_manual(values = c("en" = "red", "ru" = "blue", "other" = "green")) + 
  theme(legend.position = "none") + facet_wrap(~ userid, ncol = 30, scales = "free_y")
langplot_5 <- tweets %>% select(c(userid, tweet_language, tweet_time)) %>% filter(userid %in% users$userid[2401:3000]) %>%
  mutate(tweet_language =  ifelse(tweet_language == "en" | tweet_language == "ru", tweet_language, "other")) %>%
  ggplot(aes(x = as.Date(tweet_time), fill = tweet_language)) + geom_histogram() +
  scale_fill_manual(values = c("en" = "red", "ru" = "blue", "other" = "green")) + 
  theme(legend.position = "none") + facet_wrap(~ userid, ncol = 30, scales = "free_y")
langplot_6 <- tweets %>% select(c(userid, tweet_language, tweet_time)) %>% filter(userid %in% users$userid[3001:3608]) %>%
  mutate(tweet_language =  ifelse(tweet_language == "en" | tweet_language == "ru", tweet_language, "other")) %>%
  ggplot(aes(x = as.Date(tweet_time), fill = tweet_language)) + geom_histogram() +
  scale_fill_manual(values = c("en" = "red", "ru" = "blue", "other" = "green")) + 
  theme(legend.position = "none") + facet_wrap(~ userid, ncol = 30, scales = "free_y")
# ODER: load("user-languages_ggplot.RData")


print(langplot_1)
print(langplot_2)
print(langplot_3)
print(langplot_4)
print(langplot_5)
print(langplot_6)
#Relative Einheitlichkeit über die Zeit für alle Accounts. Vereinzelte russische Tweets in dominant englischen Accounts und anders herum, aber keine systemischen Veränderungen sichtbar. Zusätzlich zeigt sich, dass viele Accounts nur für vergleichsweise kurze Zeit aktiv waren. Auch scheint immer wieder ein kleiner Anzeigefehler aufzutauchen, dieser wirkt sich aber bei genauerer Betrachtung nicht wirklich auf die sichtbaren Ergebnisse aus.


### Tweets vs. Retweets

#Zwei Graphen: Tweets/Retweets vs. Followerzahl, Tweets/Retweets vs. Tweetzahl 