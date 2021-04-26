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
# 4. Plotting (repetisjon)
# 5. Multippel regresjon (nytt)


#### 1. Laste inn data ####

## Setter working directory (hvis du ikke jobber i prosjekt)
setwd("")

## Laster inn pakker
library(tidyverse)
library(stargazer)
library(haven)

## Laster inn datasettet
data <- read_dta("ESS8SE.dta")

## Får oversikt over data 
____(data)
____(data)


#### 2. Subsetting ####

## Tar kun med variablene vi trenger til regresjonen
data2 <- data %>%
  ______(gndr, agea, eduyrs, nwspol, stfdem, vote)

View(data2)

## Sjekker omfanget av missing verdier. 
_____(___________(data2))
____(is.na(data2))

## Fjerner NA observasjoner
data2 <- data2 %>%
  ________


#### 3. Omkoding ####

## AV: stfdem
____(data2$stfdem)
summary(data2$stfdem)

## gndr 
____(data2$gndr)

# Omkoder fra 1 = mann og 2 = kvinne, til 0 = mann og 1 = kvinne.
data2 <- data2 %>%
  ______(______ = ifelse(____ == __, __, __))

# Sjekker at omkodingen ser ok ut
table(data2$____, data2$_______)

## vote  
table(data2$vote)

# Omkoder til dikotom variabel 
data2 <- data2 %>%
  mutate(_______ = ifelse(____ __ __, __, __))
# Respondentene som har kategorien 2 eller høyere (>=) får verdien 0 
# (stemte ikke), mens respondentene med kategorien 1 får 1 (stemte)

# Sjekker at omkodingen ser ok ut
table(data2_ ____)


#### 4. Plotting ####

## Spredningsplot med regresjonslinje
ggplot(_____, aes(x = ____, y = _____)) + 
  geom_point() +
  ______________


#### 5. Multippel regresjon ####

## Kjører bivariat regresjon først, der AV: stfdem og UV: eduyrs
mod1 <- __(_____ _ ____, 
           data = data2)

summary(mod1)

__________(mod1, 
          type = "text")

## Så, multivariat regresjon der vi legger til 4 kontrollvariabler 
mod2 <- lm(stfdem ~ eduyrs _ 
             ______ _ 
             ______ _
             ______ _
             ______, 
           data = data2)

summary(mod2) 

stargazer(mod2, 
          type = "text")

## Legger til et samspillsledd mellom eduyrs og newspol med * 
mod3 <- lm(stfdem ~ eduyrs _ nwspol + 
             vote_new +
             agea + 
             gndr_new, 
           data = data2)

summary(mod3)

stargazer(mod3, 
          type = "text")
# Merk at vi får koeffisienter for både samspillsleddet og de to 
# variablene individuelt

## Presenterer resultatet av mod1 og mod2 i stargazer
stargazer(____ _ ____, # Legger til begge modellene
          type = "text") # Spesifiserer tabell-type

## Stargazer i html-format
stargazer(mod1, mod2, 
          type = "html", 
          out = "regresjonstabell.html", # Spesifisere filnavn 
          _____ = "Regresjonstabeller", # Tittel på tabellen
          _____________ = c("Utdanning", # Forklarende navn på variabler
                               "Politiske nyheter",
                               "Stemte ved forrige valg", # Navn i rekkefølge 
                               "Alder",
                               "Kjønn"),
          ______________ = "Tilfredshet med demokratiet") # Forklarende navn på AV






