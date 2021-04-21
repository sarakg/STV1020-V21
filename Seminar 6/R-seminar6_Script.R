###########################
##                       ## 
##  Script R-seminar 6   ##
##   STV1020 Vår 2021    ##
##                       ##
###########################

#### Multippel regresjon ####

# 1. Laste inn data (repetisjon)
# 2. Subsetting (repetisjon)
# 3. Omkoding (repetisjon)
# 4. Multippel regresjon (nytt)
# 5. Plotting (repetisjon + nytt)

# Bruk hjelpefilene flittig dersom du lurer på hvordan ulike funksjoner brukes


#### 1. Laste inn data ####

## Setter working directory (hvis du ikke jobber i prosjekt)
setwd("")

## Laster inn pakker
library(tidyverse)
library(stargazer)
library(haven)

## Laster inn datasettet. Bruker funksjonen read_dta fra haven pakken.
data <- read_dta("ESS8SE.dta")

## Får oversikt over data 
View(data)
head(data)


#### 2. Subsetting ####

## Tar kun med variablene vi trenger til regresjonen
data2 <- data %>%
  select(gndr, agea, eduyrs, nwspol, stfdem, vote)

## Bruker table() og complete.cases() for å sjekke omfanget av missing verdier. 
table(complete.cases(data2))

## Fjerner NA observasjoner
data2 <- data2 %>%
  drop_na()


#### 3. Omkoding ####

## AV: demokrati
str(data2$stfdem)
summary(data2$stfdem)
## Hvilket målenivå har denne variabelen? Må vi omkode?

## Omkoder gndr fra 1 = mann og 2 = kvinne, til 0 = mann og 1 = kvinne.

table(data2$gndr)

data2 <- data2 %>%
  mutate(gndr_new = ifelse(gndr == 1, 0, 1))

# Sjekker at omkodingen ser ok ut
table(data2$gndr, data2$gndr_new)

## Omkoder vote  
table(data2$vote)

## Dikotom variabel for hvorvidt respondentene stemte ved siste valg eller ikke
data2 <- data2 %>%
  mutate(vote_new = ifelse(vote >= 2, 0, 1))
# Respondentene som har verdien 2 eller høyere får verdien 0, mens respondentene
# med 1 får 1

# Sjekker at omkodingen ser ok ut
table(data2$vote_new)


#### 4. Multippel regresjon ####

## Kjører bivariat regresjon først, der AV: stfdem og UV: vote_new
mod1 <- lm(stfdem ~ vote_new, 
           data = data2)

summary(mod1)

stargazer(mod1, 
          type = "text")

## Så, multivariat regresjon der vi legger til 4 kontrollvariabler 
mod2 <- lm(stfdem ~ vote_new + 
             nwspol + 
             eduyrs +
             agea +
             gndr_new, 
           data = data2)

summary(mod2) 

stargazer(mod2, 
          type = "text")

## Legger til et samspillsledd mellom vote_new og nwspol med * 
mod3 <- lm(stfdem ~ vote_new * nwspol + 
             eduyrs +
             agea + 
             gndr_new, 
           data = data2)

summary(mod3)

stargazer(mod3, 
          type = "text")
# Merk at vi får koeffisienter for både samspillsleddet og de to variablene 
# individuelt


#### 5. Plotting ####

## Spredningsplot med regresjonslinje
ggplot(data2, aes(x = eduyrs, y = stfdem)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red")

## Presenterer resultatet av mod1 og mod2 i stargazer
stargazer(mod1, mod2, # Legger til begge modellene
          type = "text", # Spesifiserer tabell-type
          title = "Regresjonstabeller", # Tittel på tabellen
          covariate.labels = c("Stemte ved forrige valg", # Forklarende navn på variabler
                               "Politiske nyheter",
                               "Utdanning",# Navn i rekkefølge som i regresjonsformelen
                               "Alder",
                               "Kjønn"),
          dep.var.labels = "Tilfredshet med demokratiet") # Forklarende navn på AV

## Stargazer i html-format
stargazer(mod1, mod2, 
          type = "html", 
          out = "regresjonstabell.html", # Spesifisere filnavn 
          title = "Regresjonstabeller", 
          covariate.labels = c("Stemte ved forrige valg", 
                               "Politiske nyheter",
                               "Utdanning",
                               "Alder",
                               "Kjønn"),
          dep.var.labels = "Tilfredshet med demokratiet")
          





