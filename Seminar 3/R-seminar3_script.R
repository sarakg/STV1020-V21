##########################
##                      ##
##  Script R-seminar 2  ##
##   STV1020 Vår 2021   ##
##                      ##
##########################

#### Tema for seminar ####

# 1. Mappestruktur og working directory
# 2. Laste inn data
# 3. Omkoding av variabler 
# 4. Subsetting av datasett 
# 5. Plotting med ggplot 
# 6. Ulike typer plott
# 7. Lagring av plott

#### 1. Repetisjon av mappestruktur ####

## Først fjerner vi alt som er i environment 
rm(list = ls())
# Kan alternativt trykke på "feiekostsymbolet" i environment. 
# For å fjerne kun ett element skriver vi: rm(navnetpåelementet)

## Setter en working directory
setwd("filstiendin")

#### 2. Laste inn data ####

# Installerer pakken og henter den fra bibiloteket
install.packages("gapminder")
library(gapminder) 

## Oppretter et objekt av datasettet 
gapminder <- gapminder

## Lagrer data i R format, med formål å laste det ned ved hjelp av load()-funksjonen
save(gapminder, file = "gapminder.Rdata")

## Laster inn det lagrede datasettet fra wd 
rm(gapminder) # Fjerner det eksisterende datasettet
load("gapminder.Rdata")
# Når vi har R-data (ikke csv, dta osv.) holder det å bare bruke load()-funksjonen


#### 3. Omkoding av variabler #####

## Får oversikt over data 
names(gapminder)
head(gapminder)
summary(gapminder)
View(gapminder)

## Matematisk omkoding
# Lager ny variabel year_1952 der 1952 er 0
summary(gapminder$year) # Ser at den går fra 1952 til 2007
gapminder$year_1952 <- gapminder$year - 1952 
summary(gapminder$year_1952) # Sjekker summary statistics

## Snur skalaretningen
# Lager ny variabel year_2007 der 2007 er 0, og 1952 55
gapminder$year_2007 <- gapminder$year_1952*(-1) + 55 # ganger med -1 og legger til maksimumsobservasjonen
summary(gapminder$year_2007) # Sjekker summary statistics

## Omkoding med ifelse() 
# Lager ny variabel year_0 der vi koder år 1952 om til 0
gapminder$year_0 <- ifelse(gapminder$year == 1952, 0, gapminder$year)
summary(gapminder$year_0) # Sjekker summary statistics
table(gapminder$year_0)

## Omkoder lifeExp med ifelse()
summary(gapminder$lifeExp)

# Gir verdien 1 til alle observasjoner med lifeExp under gjennomsnittet, og beholder
# lifeExp verdiene til observasjonene med lifeExp over gjennomsnittet
gapminder$lifeExp_2 <- ifelse(gapminder$lifeExp < 59.47, 1, gapminder$lifeExp)

# Gir verdien 1 til alle observasjoner med lifeExp under gjennomsnittet, og 0 til 
# observasjonene over gjennomsnittet
gapminder$lifeExp_2 <- ifelse(gapminder$lifeExp < 59.47, 1, 0)

## Omkoder til dummyvariabel for kategorisk variabel med ifelse()
table(gapminder$continent)

# Gir "Asia" verdien 1, og alle andre kontinenter verdien 0
gapminder$continent_dummy <- ifelse(gapminder$continent == "Asia", 1, 0)

table(gapminder$continent_dummy, gapminder$continent)


#### 4. Subsetting av datasett #####

## Henter pakke fra library 
library(tidyverse)

## Velger ut relevante variabler fra datasettet
gapminder_subset <- gapminder %>%
  select(pop, country, year)

## Filtrerer på år og populasjon
gapminder_subset <- gapminder_subset %>% 
  filter(year == 1952, 
         pop > mean(pop))

## Dette kan vi gjøre i en operasjon, slik: 
gapminder2 <- gapminder %>% 
  select(pop, country, year) %>%
  filter(year == 1952, 
         pop > mean(pop))


#### 5. Plotting med ggplot ####

## Vi bygger plottene lag for lag: 

# Først forteller vi hvilke data vi vil bruke.
# Så hva vi vil vite på x-aksen eller x- og y-aksen. 
# Så forteller vi hva slags plot vi vil lage. 
ggplot(gapminder, aes(x =  continent)) +
  geom_bar() # kategorisk variabel

# Vi kan legge inn "fill" i aesthetics for å skille kontinentene med ulike farger:
ggplot(gapminder, aes(x = continent, fill = continent)) + 
  geom_bar()

# Vi kan endre teksten til x- og y-aksen, og gi plottet en tittel:
ggplot(gapminder, aes(x=continent, fill=continent)) + 
  geom_bar() +
  labs(x = "Kontinenter", 
       y = "Antall land/år observasjoner", 
       title = "Søylediagram")


#### 6. Ulike typer plott ####

## 1. Histogram over forventet levealder
ggplot(gapminder, aes(lifeExp)) +
  geom_histogram() # kontinuerlig variabel

# Vi setter binwidth til 1 (ett år per stolpe)
ggplot(gapminder, aes(lifeExp)) +
  geom_histogram(binwidth = 1)

# Vi kan hente ut enda mer info ved å legge inn fill: 
ggplot(gapminder, aes(lifeExp, fill = continent)) +
  geom_histogram(binwidth = 1)

## 2. Box plot over forventet levealder per kontinent:
ggplot(gapminder, aes(x = continent, y = lifeExp, fill = continent)) + 
  geom_boxplot() # én kategorisk og én kontinuerlig variabel

## 3. Density plot/tetthetsplott
ggplot(gapminder, aes(lifeExp)) + 
  geom_density() # kontinuerlig variabel

# Legger til spesifikasjoner om tykkelse, farge osv. i geom_density: 
ggplot(gapminder, aes(lifeExp)) + 
  geom_density(size = 1.5, fill = "pink", alpha = 0.3)

# Hva skjer hvis vi endrer alpha til 1 og size til 0.5?
ggplot(gapminder, aes(lifeExp)) + 
  geom_density(size = 0.5, fill = "pink", alpha = 1)

# Separerer plottene ved bruk av facet_wrap: 
ggplot(gapminder, aes(lifeExp)) + 
  geom_density(size = 0.5, fill = "pink", alpha = 1) + 
  facet_wrap(vars(continent))

## 4. Scatterplot over to kontinuerlige variabler

# Legger inn informasjon om både x- og y-aksen, samt geom_point: 
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) + 
  geom_point()

# Legger til linje som viser gjennomsnittet i observasjonene: 
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_point() + 
  geom_smooth()

# Skiller kontinentene fra hverandre med farger:
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth() 

# Facet wrap for å skille plottene, med svart farge på geom_smooth:
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth(color = "black") + 
  facet_wrap(vars(continent)) 

# Ny tekst til x- og y-aksen, samt tittel
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth(colour = "black") + 
  facet_wrap(vars(continent)) +
  labs(x = "Forventet levealder", 
       y = "BNP per innbygger", 
       title = "Et scatterplot med Gapminder-data") 


#### 7. Lagring av plott ####

# Lage objekter av plott: 
p <- ggplot(gapminder, aes(lifeExp, fill=continent)) +
  geom_histogram(binwidth = 1)

# Bygge videre på plott:
p + labs(title = "Et plot med Gapminderdata")
  
# Bruk ggsave, gi plottet et navn, og spesifiser formatet du vil lagre i. 
# Det blir lagra i ditt working directory. 

# Som .png
ggsave("gdplevealder.png")

# Som .pdf
ggsave("gdplevealder.pdf")


