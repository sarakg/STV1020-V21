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
setwd(_______)

## Laster inn pakker
library(________)
library(________)

## Laster inn datasettet
load("________")

#### 1.1 Undersøker data ####

## Ser på datasettet
View(FairFPSR3)

## Henter ut informasjon om variabelnavn, klasse m.m.
____(FairFPSR3)

## Printer variabelnavnene
____(FairFPSR3)

#### 1.2 Missing ####

## Missing i datasettet
table(_______(FairFPSR3)) # 1 rad 
sum(____(FairFPSR3)) # 2 NA-verdier 

## Missing på variabelen inflation
_____(is.na(FairFPSR3$_______)) # 1 rad
____(is.na(FairFPSR3$_______)) # 1 NA-verdi

## Lager to nye variabler, complete og if_na
FairFPSR3 <- FairFPSR3 %>% 
  ______(complete = complete.cases(___),
         if_na = is.na(______))


#### 2. Omkoding av variabler ####

## Oppretter ny variabel basert på variabelen growth
FairFPSR3 <- FairFPSR3 %>% 
  mutate(growth_dich = _____(growth > ___, "Growth", "No growth"), 
         growth_dich = _____(growth_dich, _____ = c("No growth", "Growth")))
# endrer referansekategori

## Sjekker at det ser ok ut: 
_____(FairFPSR3$growth_dich)
table(FairFPSR3$growth_dich, ____ = "_______")

## Plotting av growth, fylt med farge for growth, ikke-growth 
ggplot(FairFPSR3, aes(x = _____, fill = _________)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Growth rate", 
       y = "Number of observations") + 
  _____() +
  theme(_____.____ = element_blank()) 


#### 3. Plotting ####

## Sammenhengen mellom økonomisk vekst og andel som stemte på det sittende partiet
ggplot(FairFPSR3, aes(x = _____, y = ________)) +
  geom_point() +
  labs(x = "Percentage Change in Real GDP Per Capita", 
       y = "Incumbent-Party Vote Percentage", 
       title = "Scatterplot of change in GDP and incumbent party-vote share") +
  theme_bw()


#### 4 og 5. Regresjonsmodell, fremstilling og tolkning ####

## Regresjon med numerisk UV
model <- __(______ ~ ______, 
            _____ = FairFPSR3, 
            na.action = "na.exclude")

summary(model) 

# Printer resultat som tekst
stargazer(model, 
          type = "_____")

# Printer resultat som html-type
stargazer(model, 
          type = "_____",
          out = "________.___")

## Plotter regresjonslinje og støttelinjer i plottet fra i stad
ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point() +
  labs(x = "Percentage Change in Real GDP Per Capita", 
       y = "Incumbent-Party Vote Percentage", 
       title = "Scatterplot of change in GDP and incumbent party-vote share") +
  theme_bw() +
  _________(method = "___") + # Legger til regresjonslinje
  geom_hline(yintercept = ____(FairFPSR3$inc_vote), linetype = "dashed") +
  geom_vline(xintercept = ____(FairFPSR3$growth), linetype = "dashed")
# Legger til stiplede linjer som viser gjennomsnittet til x- og y-variabelen

# Gjennomsnittet til x og y
mean(FairFPSR3$growth)
mean(FairFPSR3$inc_vote)

# Konfidensintervall til regresjonsmodellen vår
______(model)

## Regresjon med dikotom uavhengig variabel
summary(model2 <- lm(inc_vote ~ _________, 
                         data = FairFPSR3))

# Printer resultatet
stargazer(________, 
          type = "text")
