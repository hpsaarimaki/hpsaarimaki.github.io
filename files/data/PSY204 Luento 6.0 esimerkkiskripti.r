# PSY204
# Luento 6.0: Kommentteja lineaarisen regression kotitehtäviin
# Heini Saarimäki 9.10.2022

# ---

# 1. Aineiston valmistelu

# Asetetaan työskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/Kotitehtävät")

# Ladataan valmis aineisto:
suvilehto <- read.csv('https://hpsaarimaki.github.io/files/data/suvilehto_clean.csv', sep=";")
suvilehto$X <- NULL # poistan ylimääräisen sarakkeen

# Muutetaan faktorit faktoreiksi:
suvilehto$subid <- factor(suvilehto$subid)
suvilehto$sex <- factor(suvilehto$sex)
suvilehto$country <- factor(suvilehto$country)
suvilehto$person <- factor(suvilehto$person)

# Kiinnitetään aineisto:
attach(suvilehto)

# ---

# 2. Logistinen regressio

# Kokeillaan dikotomisoida kosketus

# Kasvot
suvilehto$facebin <- 0
suvilehto[which(suvilehto$face>0),11] <- 1
suvilehto[is.na(suvilehto$face),11] <- NA

# Kädet
suvilehto$handbin <- 0
suvilehto[which(suvilehto$hand>0),12] <- 1
suvilehto[is.na(suvilehto$hand),12] <- NA

# Jalat
suvilehto$legbin <- 0
suvilehto[which(suvilehto$leg>0),13] <- 1
suvilehto[is.na(suvilehto$leg),13] <- NA

# Muutetaan dikotomiset muuttujat faktoreiksi
suvilehto$facebin <- factor(suvilehto$facebin)
suvilehto$handbin <- factor(suvilehto$handbin)
suvilehto$legbin <- factor(suvilehto$legbin)

summary(suvilehto)

attach(suvilehto)

# Vertaillaan histogrammeja ja frekvenssikuvioita:
par(mfrow=c(3,2)) 
hist(face)
barplot(table(facebin), main="Frequency plot of face")
hist(hand)
barplot(table(handbin), main="Frequency plot of hand")
hist(leg)
barplot(table(legbin), main="Frequency plot of leg")

# -

# Logistinen regressio

# Poistetaan  rivit joissa on puuttuvia arvoja:
suvilehto.logreg <- suvilehto[complete.cases(suvilehto),]

# Aineiston jakaminen
opetus <- suvilehto.logreg[1:6000,]
testaus <- suvilehto.logreg[6001:7379,]

# Kasvot - sovitetaan malli:
malli1 <- glm(facebin ~ country + bond, family=binomial, data=opetus)
malli2 <- glm(facebin ~ country * bond, family=binomial, data=opetus)

# Yksittäisten selittäjien merkitys
summary(malli1)
summary(malli2)

# Mallin sopivuus
anova(malli1, test="Chisq")
anova(malli2, test="Chisq")

# Mallien vertailu
library(AICcmodavg)
aictab(cand.set = list(malli1, malli2), modnames = c("malli1", "malli2") )
# malli 2 eli yhdysvaikutukset sisältävä malli on parempi

# Mallin selitysaste
# Nagelkerken pseudo-R^2
library(fmsb)
NagelkerkeR2(malli1)
NagelkerkeR2(malli2)

# Mallin ennustustarkkuuden arviointi
ennusteet1 <- predict(malli1, newdata=subset(testaus, select=c(4,6)), type="response")
ennusteet1 <- ifelse(ennusteet1 > 0.5, 1, 0)
ennusteet2 <- predict(malli2, newdata=subset(testaus, select=c(4,6)), type="response")
ennusteet2 <- ifelse(ennusteet2 > 0.5, 1, 0)

# Luokitteluvirheet paremmalle mallille
luokitteluvirheet <- mean(ennusteet2 != testaus$facebin, na.rm=T)
print(paste("Tarkkuus:", 1-luokitteluvirheet))
# virheitä n. 0.75

# Ristiintaulukointi:
testaus$face_predicted <- factor(ennusteet2)  
ennustejakauma_face <-prop.table(table(testaus$face_predicted,
                                       testaus$facebin),2)*100
round(ennustejakauma_face)

#Kuvio:
barplot(ennustejakauma_face, legend=TRUE, main="Ennustejakauma: kasvot", 
        xlab="Todellinen luokka", ylab="Ennuste")
# Huom. malli ennustaa aina luokkaa 1!

# -

# Kädet - sovitetaan malli:
malli3 <- glm(handbin ~ country + bond, family=binomial, data=opetus)
malli4 <- glm(handbin ~ country * bond, family=binomial, data=opetus)

# Yksittäisten selittäjien merkitys
summary(malli3)
summary(malli4)

# Mallin sopivuus
anova(malli3, test="Chisq")
anova(malli4, test="Chisq")
aictab(cand.set = list(malli3, malli4), modnames = c("malli3", "malli4") )

# malli 4 parempi

# Mallin selitysaste
# Nagelkerken pseudo-R^2
library(fmsb)
NagelkerkeR2(malli3)
NagelkerkeR2(malli4)

# Mallin ennustustarkkuuden arviointi
ennusteet3 <- predict(malli3, newdata=subset(testaus, select=c(4,6)), type="response")
ennusteet3 <- ifelse(ennusteet3 > 0.5, 1, 0)
ennusteet4 <- predict(malli4, newdata=subset(testaus, select=c(4,6)), type="response")
ennusteet4 <- ifelse(ennusteet2 > 0.5, 1, 0)

# Luokitteluvirheet paremmalle mallille
luokitteluvirheet <- mean(ennusteet4 != testaus$handbin, na.rm=T)
print(paste("Tarkkuus:", 1-luokitteluvirheet))
1-luokitteluvirheet

# Ristiintaulukointi:
testaus$hand_predicted <- factor(ennusteet4)  
ennustejakauma_hand <-prop.table(table(testaus$hand_predicted,
                                       testaus$handbin),2)*100
round(ennustejakauma_hand)
# virheitä n. 0.74

#Kuvio:
barplot(ennustejakauma_hand, legend=TRUE, main="Ennustejakauma: kädet", 
        xlab="Todellinen luokka", ylab="Ennuste")
# malli ennustaa aina, että käsiä saa koskea (eli luokkaa 1)


# -

# Jalat - sovitetaan malli:
malli5 <- glm(legbin ~ country + bond, family=binomial, data=opetus)
malli6 <- glm(legbin ~ country * bond, family=binomial, data=opetus)

# Yksittäisten selittäjien merkitys
summary(malli5)
summary(malli6)

# Mallin sopivuus
anova(malli5, test="Chisq")
anova(malli6, test="Chisq")
aictab(cand.set = list(malli5, malli6), modnames = c("malli5", "malli6") )

# malli 6 parempi

# Mallin selitysaste
# Nagelkerken pseudo-R^2
library(fmsb)
NagelkerkeR2(malli5)
NagelkerkeR2(malli6)

# Mallin ennustustarkkuuden arviointi
ennusteet5 <- predict(malli5, newdata=subset(testaus, select=c(4,6)), type="response")
ennusteet5 <- ifelse(ennusteet5 > 0.5, 1, 0)
ennusteet6 <- predict(malli6, newdata=subset(testaus, select=c(4,6)), type="response")
ennusteet6 <- ifelse(ennusteet6 > 0.5, 1, 0)

# Luokittelutarkkuus paremmalle mallille
luokitteluvirheet <- mean(ennusteet6 != testaus$legbin, na.rm=T)
print(paste("Tarkkuus:", 1-luokitteluvirheet))
1-luokitteluvirheet
# virheitä n. 0.66

# Ristiintaulukointi:
testaus$leg_predicted <- factor(ennusteet6)  
ennustejakauma_leg <-prop.table(table(testaus$leg_predicted,
                                       testaus$legbin),2)*100
round(ennustejakauma_leg)

#Kuvio:
barplot(ennustejakauma_leg, legend=TRUE, main="Ennustejakauma: jalat", 
        xlab="Todellinen luokka", ylab="Ennuste")
# Enemmän virheitä luokan 0 ennustamisessa

# ---

# 3. Lineaarinen sekamalli

library(lme4)
library(lmerTest)

# Kasvot - luodaan mallit, osallistujatunnus satunnaismuuttujana
malli.1 <- lmer(face ~ country + bond + (1 | subid), data=suvilehto)
summary(malli.1)
malli.2 <- lmer(face ~ country * bond + (1 | subid), data=suvilehto)
summary(malli.3)

# Mallien vertailu
anova(malli.1, malli.2)
# kulttuurin ja siteen yhdysvaikutus ei enää merkitsevä

# -

# Kädet - luodaan mallit:
malli.3 <- lmer(hand ~ country + bond + (1 | subid), data=suvilehto)
summary(malli.3)
# kulttuuri ei merkitsevä

malli.4 <- lmer(hand ~ country * bond + (1 | subid), data=suvilehto)
summary(malli.4)
anova(malli.4)

# Mallien vertailu
anova(malli.3, malli.4)
# kulttuurin yhdysvaikutus ja päävaikutus tässä merkitsevä

# -

# Jalat - luodaan mallit:
malli.5 <- lmer(leg ~ country + bond + (1 | subid), data=suvilehto)
summary(malli.5)
malli.6 <- lmer(leg ~ country * bond + (1 | subid), data=suvilehto)
summary(malli.6)

# Mallien vertailu
anova(malli.5, malli.6)
# kulttuurin ja tunnesiteen päävaikutus merkitsevä
