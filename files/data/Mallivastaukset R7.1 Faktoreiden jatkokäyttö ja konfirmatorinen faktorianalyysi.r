# PSY204 syksy 2022
# Harjoitukset 7.1: Faktoreiden jatkokäyttö ja konfirmatorinen faktorianalyysi
# Mallivastaukset
# Heini Saarimäki 25.10.2022
# 
# ----

# Ladataan tarvittavat kirjastot
library(lavaan)

# Asetetaan työskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ---

# Ladataan tarvittavat kirjastot
library(psych)
library(GPArotation)

# Asetetaan työskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:

tas <- read.csv("2022-2023/PSY204 - syksy 2022/Materiaalit/briganti-tas-data-2.csv", sep=";")

# Tarkastellaan aineistoa:
summary(tas)

# Tarkistetaan puuttuvat havainnot:
describe(tas)
anyNA(tas) 

# Kiinnitetään tietokehys
attach(tas)

# ---

# 2. Eksploratiivinen faktorianalyysi

# Tallennetaan viime viikolta tuttu kolmen faktorin malli.
# Huom. valitse vain tas-muuttujat!
# Ajetaan kolmen faktorin malli:
fa3 <- fa(tas[1:20],
          nfactor=3,
          fm="pa",
          max.iter=100,
          rotate="oblimin")

# Visuaalinen tarkastelu:
fa.diagram(fa3)

# Varmista, että kaikki lataukset positiivisia eli että
# aineistossa ei ole käänteisiä muuttujia jäljellä!

# ---

# 3. Yhdistelmämuuttujat

# 3.1 Summamuuttujat

# Kokonaispistemäärä
tas$TOTAL <- rowSums(tas[1:20])

# Summamuuttujat
tas$DIF <- rowSums(tas[c(1, 3, 6, 7, 9, 13, 14)])
tas$DDF <- rowSums(tas[c(2, 4, 11, 12, 17)])
tas$EOT <- rowSums(tas[c(5, 8, 10, 15, 16, 18, 19, 20)])

# -

# Kuvailevat tulokset summamuuttujille:
describe(tas[27:30])

# Summamuuttujien jakaumat:
par(mfrow=c(1,4))
hist(tas$DIF)
hist(tas$DDF)
hist(tas$EOT)
hist(tas$TOTAL)

# -

# Cronbachin alfat summamuuttujille:
library(ltm)

# Kokonaispistemäärä:
cronbach.alpha(tas[1:20], CI=TRUE)

# Summamuuttujat:
cronbach.alpha((tas[c(1, 3, 6, 7, 9, 13, 14)]), CI=TRUE)
cronbach.alpha(tas[c(2, 4, 11, 12, 17)], CI=TRUE)
cronbach.alpha(tas[c(5, 8, 10, 15, 16, 18, 19, 20)], CI=TRUE)

# -

# Faktoreiden jatkokäyttö:

# Selittävätkö DIF, DDF ja EOT muuttujan sim_A arvoja?

malli1 <- lm(sim_A ~ DIF + DDF + EOT, data=tas)
summary(malli1)


# -

# 3.2 Faktoripisteisiin perustuvat yhdistelmämuuttujat

# Oletusarvona funktiossa fa oli faktoripisteiden laskeminen regressiolla:
tas <- cbind(tas, fa3$scores)

# Faktoripistemuuttujien jakaumat:
par(mfrow=c(1,3))
hist(tas$PA1)
hist(tas$PA2)
hist(tas$PA3)

# Voidaan laskea myös tenBerge, Anderson tai Bartlett muokkaamalla
# faktorianalyysin ajamista, esim:

fa3.tenBerge <- fa(tas[1:20],
                   scores="tenBerge",
                   nfactor=3,
                   fm="pa",
                   max.iter=100,
                   rotate="oblimin")

tas <- cbind(tas, fa3.tenBerge$scores)
colnames(tas)[34] <- "PA1.tenBerge"
colnames(tas)[35] <- "PA3.tenBerge"
colnames(tas)[36] <- "PA2.tenBerge"

# Faktoripistemuuttujien jakaumat:
par(mfrow=c(1,3))
hist(tas$PA1.tenBerge)
hist(tas$PA2.tenBerge)
hist(tas$PA3.tenBerge)

# ---

# 4. Konfirmatorinen faktorianalyysi

## 4.1 Aineiston valmistelu

# Ladataan aineisto:

tas2 <- read.csv("2022-2023/PSY204 - syksy 2022/Materiaalit/tas-scores.csv", sep=";")

# Tarkastellaan aineistoa:
summary(tas2)
dim(tas2)

# Muutetaan faktorit faktoreiksi:
tas2$ID <- factor(tas2$ID)
tas2$Sex <- factor(tas2$Sex)

# Onko puuttuvia arvoja?
which(is.na(tas2))

# Aineistossa on joitakin puuttuvia arvoja. Poistetaan rivit.
tas2 <- tas2[complete.cases(tas2), ]

# Tarkastellaan tietokehystä:
describe(tas2)
dim(tas2)

# -

# 4.2 Aineiston tarkastelu

# Tarkistetaan TAS-muuttujien korrelaatiomatriisi:
library(GGally)
ggcorr(tas2[3:22]) 

# Käänteisiä muuttujia on. Tarkastellaan mitkä ne olivat ja käännetään ne.
# Five of the items are reverse-scored: 4, 5, 10, 18, and 19.
tas2$tas4 <- 6-tas2$tas4
tas2$tas5 <- 6-tas2$tas5
tas2$tas10 <- 6-tas2$tas10
tas2$tas18 <- 6-tas2$tas18
tas2$tas19 <- 6-tas2$tas19


# Jakaumat
par(mfrow=c(1,5))
hist(tas2$tas1)
hist(tas2$tas2)
hist(tas2$tas3)
hist(tas2$tas4)
hist(tas2$tas5)

# -

# 4.3 Mallin rakentaminen

polku <- '
  dif = ~ tas1 + tas3 + tas6 + tas7 + tas9 + tas13 + tas14
  ddf = ~ tas2 + tas4 + tas11 + tas12 + tas17
  eot = ~ tas5 + tas8 + tas10 + tas15 + tas16 + tas18 + tas19 + tas20'

# -

# 4.4 Mallin sovittaminen ja tulosten tarkastelu

cfa.malli <- cfa(polku, data = tas2[3:22])
summary(cfa.malli, fit.measures = TRUE)


