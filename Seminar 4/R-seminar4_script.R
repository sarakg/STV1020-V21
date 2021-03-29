##########################
##                      ##
##  Script R-seminar 4  ##
##   STV1020 Vår 2021   ##
##                      ##
##########################

#### Tema for seminar ####

# 1. Laste inn data
# 2. Missing
# 3. Statistiske mål
# 4. Univariat analyse
# 5. Bivariat analyse  

#### 1. Laste inn data ####

## Setter working directory
getwd()
setwd("")

## Installerer og laster ned relevante pakker
library(tidyverse)

## Laster inn datasett fra github: https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv 
data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")

## Inspiserer datasettet
View(data)
head(data)
names(data)
summary(data) 

#### 2. Missing - NA - NOT AVAILBLE ####

## Finner antall missing i hele datasettet
sum(is.na(data)) 

## Finner antall missing på bestemte variabler
sum(is.na(data$kjonn))
sum(is.na(data$internettbruk))

# Summary gir også info om NAs
summary(data$internettbruk)

## Håndtere missing: Sjekk ut hjelpefilen for NA.
?NA

# Håndtere missing: Ulike typer og bruk av subset fra tidyverse
no_na_data <- data %>% 
  drop_na(internettbruk)

# Sjekker at det ble riktig
sum(is.na(no_na_data$internettbruk))
# Fem færre observasjoner (rader) i datasettet, fjerner hele raden med missing

#### 3. Statistiske mål ####

# Rask oversikt over viktige statistiske mål
summary(data)

# Gjennomsnitt og median
mean(data$tillit, na.rm = TRUE) # Hva skjer hvis vi ikke skriver na.rm = TRUE?
median(data$tillit, na.rm = TRUE) # Husk på missing!

# Gjennomsnitt til en dikotom variabel/variabel på nominalnivå
mean(data$kjonn, na.rm = TRUE) 
# Diskutér...

# Standardavvik, gjennomsnittlig avstand fra gjennomsnittet 
sd(data$tillit, na.rm = TRUE) # Husk på missing!

# Varians, standardavviket ^2 (lagrer i eget objekt)
varians <- var(data$tillit, na.rm = TRUE)
sqrt(varians) # Får standardavviket igjen

#### 3. Univariat analyse ####

# tibble() og str() finner ut av hva slags variabler det er i datasettet. Vi må installere tibble-pakken 
# og hente den i biblioteket
install.packages("tibble")
library(tibble)

tibble(data)
str(data)

## Kategoriske variabler ##

# Frekvenstabeller for kategoriske variabler (internettbruk)
table(data$internettbruk)

# Lagrer tabellen i et objekt
tabell <- table(data$internettbruk)
tabell

# Relativ fordeling i prosent
tabell_2 <- prop.table(table(data$internettbruk))*100
tabell_2

# Søylediagram for grafisk beskrivelse av tabellene, bruker ggplot 
ggplot(data, aes(internettbruk)) + 
  geom_bar()

## Kontinuerlige variabler ##

# Histogram for alder (kontinuerlig variabel)
ggplot(data, aes(alder)) + 
  geom_histogram(bins = 25, 
                 fill = "grey", 
                 col = "white")
# Vi kan som kjent også legge til flere argumenter. Bruk hjelpefilen

## Eksportere tabeller med deskriptiv statistikk ##

# Vi bruker pakken stargazer, som er fin for å eksportere tabeller
install.packages("stargazer")
library(stargazer)

stargazer(data, 
          type = "html", 
          out = "deskriptiv.html")

#### 5. Bivariat analyse #### 

## To kateogriske variabler ##

# Lager krysstabell for internettbruk og kjønn
krysstabell <- table(data$internettbruk, data$kjonn)
krysstabell
# Tolk tabellen

# Krystabell i relative tall 
prop.table(krysstabell, margin = 1)*100

# Kjikvadrattesten
chisq.test(krysstabell)
# X-squared 

# Søylediagram for internettbruk og kjønn
ggplot(data, aes(x = internettbruk , fill = as.factor(kjonn))) + 
  geom_bar(position = "dodge") + # "dodge", "fill", "stack"
  labs(fill = "Kjønn")

## To kontinuerlige variabler ## 

# Pearsons r for alder og utdanning.
R <- cor(x = data$alder, 
         y = data$utdanning, 
         use = "pairwise.complete.obs", # Beholder alle observasjoner med observasjoner på begge variablene
         method = "pearson") # Spesifiserer pearsons r

R
# Hva forteller dette oss?

# Korrelasjonsmatrise for hele datasettet
cor(data, 
    use = "pairwise.complete.obs")

# Spredningsdiagram 
ggplot(data, aes(alder, utdanning)) + 
  geom_point() +
  geom_smooth(method = "lm") # Med regresjonstype (lineær) støttelinje

