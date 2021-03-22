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

## Fjerner alle elementer i environment 
rm(list = ls())

## Working directory
setwd()

#### 2. Laste inn data ####

# Installerer gapminder-pakken og henter den fra bibiloteket
install.packages("")
library() 

## Oppretter et objekt av datasettet 
___ <- gapminder

## Lagrer data i R format
save(____, file = "gapminder.___")

## Laster inn det lagrede datasettet fra wd 
rm(____) 
load("gapminder.Rdata")

#### 3. Omkoding av variabler #####

## Får oversikt over data 
____(gapminder)
____(gapminder)
____(gapminder)
View(gapminder)

## Matematisk omkoding
# Lager ny variabel year_1952 der 1952 er 0
summary(gapminder$____) 
gapminder$year_1952 <- gapminder$____ - ____ 
_______(gapminder$year_1952)

# Snur skalaretningen
# Lager ny variabel year_2007
gapminder$year_2007 <- gapminder$_____*(-1) + ____ 
_______(gapminder$year_2007) 

## Omkoding med ifelse() 
# Lager ny variabel year_0 der vi koder år 1952 om til 0
gapminder$year_0 <- _____(gapminder$year == ____, 0, _______$____)
summary(gapminder$year_0)
_____(gapminder$year_0)

## Omkoder lifeExp med ifelse()
summary(gapminder$_____)

# 1 for alle observasjoner under gjennomsnittet, 
gapminder$lifeExp_2 <- ifelse(gapminder$lifeExp _ 59.47, ___, _____$_____)

# 1 for alle observasjoner under gjennomsnittet, og 0 til resten
gapminder$lifeExp_2 <- ifelse(gapminder$lifeExp _ 59.47, ___, ___)

## Omkoder kategorisk variabel til dummy variabel
table(gapminder$continent)

# Gir "Asia" verdien 1, og alle andre kontinenter verdien 0
gapminder$continent_dummy <- ____(gapminder$continent == "___", 1, 0)

table(gapminder$continent_dummy, gapminder$continent)


#### 4. Subsetting av datasett #####

library(tidyverse)

## Velger ut relevante variabler 
________ <- gapminder %>%
  ______(pop, country, year)

## Filtrerer på år og populasjon
gapminder_subset <- gapminder_subset %>% 
  _____(year __ 1952, 
        pop __ mean(pop))

## Gjøres i én operasjon: 
gapminder2 <- gapminder %>% 
  _____(pop, country, year) %>%
  _____(year == 1952, 
        pop > mean(pop))


#### 5. Plotting med ggplot ####

# Plot kategorisk variabel:
ggplot(_____, aes(x =  continent)) +
  geom____ 

# "Fyller" kontinentene med ulike farger:
ggplot(gapminder, aes(x = continent, fill = ____)) + 
  geom_bar()

# Legger til labs og tittel:
ggplot(gapminder, aes(x = continent, fill = continent)) + 
  geom_bar() +
  labs(x = "", 
       y = "", 
       title = "")


#### 6. Ulike typer plott ####

## 1. Histogram over forventet levealder
ggplot(gapminder, aes(lifeExp)) +
  geom_____

# Binwidth
ggplot(gapminder, aes(lifeExp)) +
  geom_histogram(binwidth = _)

# Fill: 
ggplot(gapminder, aes(lifeExp, fill = _____)) +
  geom_histogram(binwidth = 1)

## 2. Box plot over forventet levealder per kontinent:
ggplot(gapminder, aes(x = ______, y = _____, fill = _____)) + 
  geom_____ 

## 3. Density plot/tetthetsplott, én kontinuerlig variabel:
ggplot(gapminder, aes(lifeExp)) + 
  geom_density(aes(y = ..density..)) 

# Legger til spesifikasjoner  
ggplot(gapminder, aes(lifeExp)) + 
  geom_density(size = ___, fill = "___", alpha = ____)

# Endrer alpha og size 
ggplot(gapminder, aes(lifeExp)) + 
  geom_density(size = ___, fill = "____", alpha = ____)

# Separerer plottene ved bruk av facet_wrap
ggplot(gapminder, aes(lifeExp, fill = ______)) +
  geom_density(size = 0.5, alpha = 0.3) + 
  facet_wrap(______)

## 4. Scatterplot for to kontinuerlige variabler

# Forventet levealder og BNP per innbygger: 
ggplot(gapminder, aes(x = _____, y = ______)) + 
  geom_____

# Legger til linje som viser gjennomsnittet i observasjonene: 
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_point() + 
  __________

# Skiller kontinentene fra hverandre med farger:
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, ____ = ______)) + 
  geom_point() + 
  geom_smooth() 

# Facet wrap skiller plottene
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth(_____ = "black") + 
  ______(__(____))

# Ny tekst til x- og y-aksen, samt tittel
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth(colour = "black") + 
  facet_wrap(vars(continent)) +
  labs(x = "_______", 
       y = "______", 
       title = "______") 


#### 7. Lagring av plott ####

# Lage objekter av plott: 
__ <- ggplot(gapminder, aes(lifeExp, fill = continent)) + 
  geom_histogram(binwidth = 1)

p + ____(title = "Et plot med Gapminderdata")

# ggsave
ggsave("gdplevealder.png")
ggsave("gdplevealder.pdf")

