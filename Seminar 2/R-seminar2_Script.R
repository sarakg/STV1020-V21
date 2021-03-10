##########################
##                      ##
##  Script R-seminar 2  ##
##                      ##
##########################

#### PLAN FOR SEMINARET ####

# 0. Working directory og datasett
# 1. Pakker
# 2. Laste inn data 
# 3. Organisering av arbeidet 
# 4. Målenivå
# 5. Klasser og målenivå
# 6. Utforske data
# 7. Plotting


#### 0. Working directory og datasett #### 

# Først setter vi working directory til den mappen vi ønsker å hente og lagre 
# filer til.  

# Her kan man enten bruke "setwd"-funksjonen og kopiere filstien til der man vil 
# lagre scriptet eller så kan man trykke "Session" i verktøy-linjen - 
# "Set Working Directory" - "Choose Directory" for å velge en mappe. 

getwd()
setwd("filstien")

# Vi skal bruke et datasett fra European Social Survey Round 9 (2018), og dette
# datasettet inneholder svarene fra norske respondenter. Filen heter 
# "ESS9NO.dta" og ligger i Canvas. 

# Last ned datasettet og lagre det i samme mappe som den du har satt som working 
# directory.


#### 1. Pakker #### 

# Første gang man skal bruke en pakke må man installere den, slik vi gjorde 
# i seminar 1

install.packages("tidyverse") # Husk hermetegn!

# Så må vi hente den fra biblioteket for å fortelle R at vi ønsker å bruke 
# pakken. Dette må vi gjøre hver gang vi åpner R på nytt og ønsker å bruke 
# pakken. 

library(tidyverse)


#### 2. Laste inn data #### 

## For å laste inn en Excel-fil ## 

install.packages("readxl") 
library(readxl)

df <- read_excel("filnavn.xlsx") # Husk hermeregn rundt filnavnet til datasettet

## For å laste inn en CSV (Comma-separated values)-fil ##

df <- read.csv("filnavn.csv")  
df <- read.csv2("filnavn.csv")

## For å laste inn en STATA-fil ##

# Vi skal bruke et datasett fra en STATA-fil i dag.

install.packages("foreign")
library(foreign)

# Datasett for ESS runde 9 for Norge
df <- read.dta("ESS9NO.dta")

# Bli kjent med datastrukturen
View(df) # Åpner datasettet
names(df) # Oversikt over navnet på alle variablene
dim(df)

#### 3. Organisering av arbeidet ####

## Dersom vi vil endre eller tweake på datasettet, kan vi gjøre slik:
df <- df %>% # der vi indikerer at vi endrer df til en ny df, og det som kommer 
  # etter %>% (kalles en pipe) viser endringene vil skal gjøre, feks rename

# Bruker rename() for å endre navnene til variablene våre.

df <- df %>%  # df2
  rename(news = nwspol,
         interest = polintr, 
         age = yrbrn)

# Sjekker om det ble riktig
View(df)

# Vi har en variabel "age" som viser hvilket år respondenten er født. 
summary(df$age) 

# Vi kan forandre "age" til å vise alder og ikke hvilket år respondenten er født.
df$age <- 2018-df$age # Undersøkelsen er fra 2018.

# Sjekker om det ble riktig
summary(df$age)

# Her lager vi et nytt datasett med kun de variablene vi vil ha med oss videre.
# Da bruker vi select-funksjonen

df <- df %>% 
  select(news, interest, vote, age) 

# Sjekker
View(df)

### 4. Målenivå ### 

# 1. Nominalnivå; egenskapen kan deles i to eller flere gjensidig utelukkende 
# kategorier.

# F.eks. variabelen "vote", man har enten stemt, ikke stemt, eller så er man
# ikke berettiget til å stemme. 3 kategorier

# Hvis man har to gjensidig utelukkende kategorier så kan man bruke disse i 
# binomisk logistisk regresjonanalyse (ikke på pensum), så i dette tilfellet 
# kunne man vurdert å fjerne kategorien ikke berettiget til å stemme.

# 2. Ordinalnivå; egenskapen kan deles i to eller flere kategorier som kan 
# rangordnes. 

# F.eks. variabelen "interest", man kan være ikke interessert, lite interessert, 
# ganske interessert, eller veldig interessert. Det er rangert fra lite til veldig

# 3. Intervallnivå; egenskapen kan graderes på en skala med et tilfeldig nullpunkt, 
# der skalaenhet utgjør like mye av den underliggende egenskapen over hele 
# skalaen.

# F.eks. temperatur eller år, år har ikke et nullpunkt som er satt, men alder har det

# 4. Forholdstallsnivå; egenskapen kan graderes på en skala med et absolutt 
# nullpunkt, der en skalaenhet utgjør like mye av den underliggende egenskapen
# over hele skalaen

# F.eks. variabelen "news", som viser hvor mye tid en respondent bruker på 
# nyheter hver dag, og "age", som viser alderen til respondentene.


### 5. Klasser og målenivå ###

# Variabler på nominalnivå og ordinalnivå vil være av klassen "factor". 

class(df$vote)
class(df$interest)
is.factor(df$vote) # enklere å sjekke klasse ved å skrive class, så slipper
# vi å gjette is.factor

# Variabler på intervallnivå vil være av klassen "numeric" eller "integer", 
# det samme gjelder forholdstallnivå. Integer er heltall, men numeric kan ha 
# desimaler. 

class(df$news)
class(df$age)


### 6. Utforske data ### 

# Deskriptivt sammendrag av datasettet #
summary(df)

# Deskriptivt sammendrag av en variabel #
summary(df$vote)

# Strukturen til et objekt #
str(df)

# Nivåene til et objekt (ordinalskalert) #
levels(df$interest)

# De første eller siste delene til et objekt # 
head(df$interest)
tail(df$interest)


### 7. Plotting ###

# Hvordan kan vi visualisere hvordan fordelingen av politisk interesse er?
# Her kan vi bruke geom_bar for å lage et histogram. geom_histogram fungerer
# ikke her, da interest er ordinalskalert. geom_bar er bedre for kategoriske 
# variabler, og da får vi også merkelapper på søylene

ggplot(data = df, aes(x = interest)) + 
  geom_bar()

ggplot(data = df, aes(age)) +
  geom_histogram()


# Hvor mange innenfor hvert nivå av politisk interesse stemte?

ggplot(data = df, aes(x = interest)) + 
  geom_bar(aes(fill=vote), # "fyller" med om respondentene stemte 
           position = "dodge") # "dodge" forteller at du vil at "vote"-søylene 
                               # skal være ved siden av hverandre. 


# Hvordan fordeler respondentenes alder og tiden de bruker på nyheter seg?

ggplot(data = df, aes(x = news)) +
  geom_histogram(bins = 5) # bins sier hvor mange søyler vi skal ha

ggplot(data = df, aes(x = age)) +
  geom_histogram(binwidth = 10) # bindwidth sier hvor stor hver søyle skal være


# Hvordan fordeler tiden man bruker på nyheter på alder?
# Her kan vi bruke geom_point for å lage et spredningsplott.
# Har her to variabler, og da må vi definere hva som skal på x- og y-aksene.

ggplot(data = df, aes(x = age, y = news)) +
  geom_point(alpha = 0.2)
# Alpha gjør punktene gjennomsiktige, så jo mer solid et punkt er, jo flere 
# enheter er på dette punktet.


# Hvordan fordeler alder seg på interesse? Vi kan lage et boksplott med 
# geom_boxplot.

ggplot(data = df, aes(x = interest, y = age)) +
  geom_boxplot() 


# Hvis dere vil utforske hvordan man kan tilpasse de ulike diagrammene vi har
# sett på og mange andre, kan denne siden være nyttig: 
# https://www.r-graph-gallery.com/index.html
