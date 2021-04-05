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
data <- read.csv(___________)

## Inspiserer datasettet
View(data)
____(data)
_____(data)
______(data) 

#### 2. Missing - NA - NOT AVAILBLE ####

## Finner antall missing i hele datasettet
___(____(data)) 

## Finner antall missing på bestemte variabler
sum(is.na(data$________))
sum(is.na(data$________))

# Summary gir også info om NAs
_______(data$internettbruk)

# Droppe NAs på variabler
no_na_data <- data %>% 
  ______(_____, _____)

# Sjekker at det ble riktig
sum(is.na(______$_______))

#### 3. Statistiske mål ####

# Rask oversikt over viktige statistiske mål
summary(data)

# Gjennomsnitt og median
mean(data$tillit, ____ = TRUE) 
median(data$tillit, ____ = TRUE) 

# Standardavvik, gjennomsnittlig avstand fra gjennomsnittet 
___(data$tillit, na.rm = TRUE)

# Varians, standardavviket ^2 (lagrer i eget objekt)
varians <- ___(data$tillit, na.rm = TRUE)
____(varians) # Får standardavviket igjen

#### 3. Univariat analyse ####

# str() gir oss målenivået til variablene
str(____)

## Kategoriske variabler ##

# Frekvenstabeller for kategoriske variabler (internettbruk)
table(data$internettbruk)

# Lagrer tabellen i et objekt
tabell <- table(data$internettbruk)
tabell

# Relativ fordeling i prosent
tabell_2 <- prop.table(table(data$internettbruk))*___
tabell_2

# Alternativt:
install.packages("gmodels")
library(gmodels)

_________(data$internettbruk)

# Søylediagram for grafisk beskrivelse av tabellene, bruker ggplot 
ggplot(data, aes(_____)) + 
  geom_bar()

## Kontinuerlige variabler ##

# Histogram for alder (kontinuerlig variabel)
ggplot(data, aes(_____)) + 
  geom_histogram(bins = 20, 
                 fill = "grey", 
                 col = "white")

## Eksportere tabeller med deskriptiv statistikk ##

# Vi bruker pakken stargazer
install.packages("stargazer")
library(stargazer)

stargazer(____, 
          type = "____") # text/html

#### 5. Bivariat analyse #### 

## To kateogriske variabler ##

# Lager krysstabell for internettbruk og kjønn
krysstabell <- ____(data$_______, data$_____)
krysstabell

# Krystabell i relative tall 
prop.table(krysstabell, _____ = 1)*____

# Kjikvadrattesten
______(_______)
# X-squared 

# Søylediagram for internettbruk og kjønn
ggplot(data, aes(x = internettbruk , fill = as.factor(kjonn))) + 
  geom_bar(position = "_____") + # "dodge", "fill"
  ____(fill = "_____")

## To kontinuerlige variabler ## 

# Pearsons r for alder og utdanning
R <- ___(x = data$alder, 
         y = data$utdanning, 
         use = "_____", # Missing?
         method = "______") # Spesifiserer 
R
R^2

# Tester om korrelasjonen er statistisk signifikant
______(x = data$alder, 
         y = data$utdanning, 
         use = "______")

# Korrelasjonsmatrise for hele datasettet
cor(____, 
    use = "complete.obs")

# Spredningsdiagram 
ggplot(data, aes(alder, utdanning)) + 
  geom_point() +
  ______(method = "____") # Med regresjonstype (lineær) støttelinje

