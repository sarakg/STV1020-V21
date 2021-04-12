##########################
##                      ##
##  Script R-seminar 5  ##
##   STV1020 Vår 2021   ##
##                      ##
##########################

#### Tema for seminar ####

# 1. Laste inn data (repetisjon)
# 2. Omkoding av variabler (repretisjon)
# 3. Plotting (repetisjon)
# 4. Kjøre en regresjonsmodell med en uavhengig variabel (nytt)
# 5. Tolkning og fremstilling av regresjonsresultater (nytt)


#### 1. Laste inn data ####

## Setter working directory
setwd("C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV1020/R-seminarer/R-seminar 5")

## Laster inn pakker
library(tidyverse)
library(stargazer)

## Laster inn datasettet, må først legges i working directory
load("FairFPSR3.RData")

#### 1.1 Undersøker data ####

## Ser på datasettet
View(FairFPSR3)

## Henter ut informasjon om variabelnavn, klasse m.m.
str(FairFPSR3)

## Et alternativ til str()
FairFPSR3

## Printer variabelnavnene
names(FairFPSR3)

#### 1.2 Missing ####

## Sjekker hvor mange observasjoner (rader) som har NA
table(complete.cases(FairFPSR3)) # 1 rad 
sum(is.na(FairFPSR3)) # 2 NA-verdier 

## Sjekker hvor mange observasjoner som har missing på variabelen inflation
table(is.na(FairFPSR3$inflation)) # 1 rad
sum(is.na(FairFPSR3$inflation)) # 1 NA-verdi

## Lager to nye variabler, complete og if_na
FairFPSR3 <- FairFPSR3 %>% 
  mutate(complete = complete.cases(.),
         if_na = is.na(inflation))


#### 2. Omkoding av variabler ####

## Oppretter ny variabel basert på growth-variabelen growth
FairFPSR3 <- FairFPSR3 %>% 
  mutate(growth_dich = ifelse(growth > 0, "Growth", "No growth"),
         growth_dich = factor(growth_dich, levels = c("No growth", "Growth"))) 
# endrer referansekategori med levels og factor fordi kategorisk variabel

## Sjekker at det ser ok ut: 
class(FairFPSR3$growth_dich)
table(FairFPSR3$growth_dich, useNA = "always")


#### 3. Plotting ####

## Plotting av growth, fylt med farge for growth, ikke-growth 
ggplot(FairFPSR3, aes(x = growth, fill = growth_dich)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Growth rate", 
       y = "Number of observations") + 
  theme_bw() +
  theme(legend.title = element_blank()) # Tittel på kategorimerkene til høyre er blank

## Litt plotting før regresjon
## Sammenhengen mellom økonomisk vekst og andel som stemte på det sittende partiet
ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point() +
  labs(x = "Percentage Change in Real GDP Per Capita", 
       y = "Incumbent-Party Vote Percentage", 
       title = "Scatterplot of change in GDP and incumbent party-vote share") +
   theme_bw()


#### 4 og 5. Regresjonsmodell, fremstilling og tolkning ####

## Regresjon med numerisk UV
model <- lm(inc_vote ~ growth, 
            data = FairFPSR3,
            na.action = "na.exclude")

summary(model) 

# Printer resultat som tekst
stargazer(model, 
          type = "text")

# Printer resultat som html-type
stargazer(model, 
          type = "html",
          out = "modellen.html")

## Legger til variabler med fitted restledd
FairFPSR3 <- FairFPSR3 %>% 
  mutate(fitted = fitted(model), # fitted punkter
         residuals = resid(model)) # restledd

# Legger til regresjonslinje i plottet
ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point() +
  labs(x = "Percentage change in Real GDP Per Capita", 
       y = "Incumbent-Party Vote Percentage") + 
  theme_bw() +
  geom_line(aes(x = growth, y = fitted)) # Legger til regresjonslinje

# Legger til stiplede linjer som viser gjennomsnittet til x og y-variablene
ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point() +
  labs(x = "Percentage change in Real GDP Per Capita", 
       y = "Incumbent-Party Vote Percentage") +
  theme_bw() +
  geom_line(aes(x = growth, y = fitted)) +
  geom_hline(yintercept = mean(FairFPSR3$inc_vote), linetype = "dashed") +
  geom_vline(xintercept = mean(FairFPSR3$growth), linetype = "dashed")

# Gjennomsnittet til x og y
mean(FairFPSR3$growth)
mean(FairFPSR3$inc_vote)

# Konfidensintervall til regresjonsmodellen vår
confint(model)

## Regresjon med dikotom uavhengig variabel
summary(model_dich <- lm(inc_vote ~ growth_dich, 
                         data = FairFPSR3))

# Printer resultatet
stargazer(model_dich, 
          type = "text")
