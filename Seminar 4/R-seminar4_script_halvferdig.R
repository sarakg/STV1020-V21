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
setwd("")

## Installerer og laster ned relevante pakker
library(tidyverse)

## Laster inn datasett fra github: https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv 
data <- read.csv("")

## Inspiserer datasettet
View(data)
_____(data)
_____(data)
_______(data) 

#### 2. Missing - NA - NOT AVAILBLE ####

## Finner antall missing i hele datasettet
____(____(data)) 

## Finner antall missing på bestemte variabler
sum(is.na(data$____))
sum(is.na(data$_______))

# Summary gir også info om NAs
_______(data$internettbruk)

# Droppe NAs på variabler
no_na_data <- data %>% 
  _______(_____, _____)

# Sjekker at det ble riktig
sum(is.na(_____$_______))

#### 3. Statistiske mål ####

# Rask oversikt over viktige statistiske mål
______(data)

# Gjennomsnitt og median
mean(data$tillit, ____ = TRUE) 
median(data$tillit, ____ = TRUE) 

# Standardavvik, gjennomsnittlig avstand fra gjennomsnittet 
sdavvik <- _____(data$_____, na.rm = TRUE)

# Varians, standardavviket ^2 (lagrer i eget objekt)
varians <- ____(data$_____, na.rm = TRUE)
_____(varians) # Får standardavviket igjen
sdavvik^2

#### 3. Univariat analyse ####

# str() gir oss målenivået til variablene
str(____)

## Kategoriske variabler ##

# Frekvenstabell (internettbruk)
tabell <- _____(data$________)
tabell

# Relativ fordeling i prosent
tabell_2 <- prop.table(_____)*____
tabell_2

# Alternativt:
install.packages("gmodels")
library(gmodels)

_______(data$internettbruk)

# Søylediagram for grafisk beskrivelse 
ggplot(data, aes(internettbruk)) + 
  _______()

## Kontinuerlige variabler ##

# Histogram for alder 
ggplot(data, aes(____)) + 
  geom_histogram(bins = 20, 
                 fill = "grey", 
                 col = "white")

ggplot(data, aes(____)) + 
  geom_density()

## Eksportere tabeller med deskriptiv statistikk ##

# Vi bruker pakken stargazer
install.packages("stargazer")
library(stargazer)

stargazer(data, 
          type = "____" # text/html

#### 5. Bivariat analyse #### 

## To kateogriske variabler ##

# Lager krysstabell for internettbruk og kjønn
krysstabell <- ____(data$______, data$____)
krysstabell

# Krystabell i relative tall 
________(krysstabell, ____ = 1)*100

# Kjikvadrattesten
_______(krysstabell)
# X-squared og p-verdi

# Søylediagram for internettbruk og kjønn
ggplot(data, aes(x = ______ , fill = as.factor(____))) + 
  geom_bar(position = "____") + # "dodge", "fill"
  labs(fill = "Kjønn")

## To kontinuerlige variabler ## 

# Pearsons r for alder og utdanning
R <- ___(x = data$alder, 
         y = data$utdanning, 
         use = "______", # Missing?
         method = "_____") # Spesifiserer 
R
R^2

# Tester om korrelasjonen er statistisk signifikant
________(x = data$alder, 
         y = data$utdanning, 
         use = "______")

# Korrelasjonsmatrise for hele datasettet
cor(_____, 
    use = "__________")

# Spredningsdiagram 
ggplot(data, aes(alder, utdanning)) + 
  geom_point() +
  _______(method = "____") # Med regresjonstype (lineær) støttelinje

