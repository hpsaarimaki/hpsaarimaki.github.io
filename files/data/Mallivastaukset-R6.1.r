# PSY.204 syksy 2024
# Mallivastaukset
# Harjoitusmoniste R6.1
# Heini Saarimäki

# ---

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Harjoitukset/')

# ---

# 1. Aineiston valmistelu

# Aja viime viikon skripti
source('Mallivastaukset R5.1 Aineiston käsittely ja kategoriset muuttujat.r')

# Kiinnitä aineisto
attach(tunteet)

# ---

# 2. Aineiston tarkastelu

# Tarkista jakaumat:
par(mfrow=c(2,3))
hist(Social.loneliness, main="")
hist(Emotional.loneliness, main="")
hist(PNDL_total, main="")
hist(EPCON, main="")
hist(EPATT, main="")
hist(EPPRO, main="")

# Shapiro-Wilk-testit:
shapiro.test(Social.loneliness)
shapiro.test(Emotional.loneliness)
shapiro.test(PNDL_total)
shapiro.test(EPCON)
shapiro.test(EPATT)
shapiro.test(EPPRO)
# Mikään summamuuttujista ei ole normaalijakautunut.

# Sirontakuvio sosiaaliselle ja emotionaaliselle yksinäisyydelle:
plot(Social.loneliness, Emotional.loneliness)

# Sirontamatriisi kaikille summamuuttujille:
plot(tunteet[88:93])

# Tuunatumpi sirontamatriisi:
library(GGally)
ggpairs(tunteet[88:93]) 

# ---

# 3. Korrelaatiot

# Sosiaalisen ja emotionaalisen yksinäisyyden välinen korrelaatio:
library(psych)
cor.test(Social.loneliness, Emotional.loneliness, method="spearman")
# r=.307, p=.004
# Selitysaste:
0.307*0.307
# 0.09, eli emotionaalinen yksinäisyys selittää 9% sosiaalisen yksinäisyyden vaihtelusta

# Kaikkien summamuuttujien välinen korrelaatiomatriisi:
corr.test(tunteet[88:93], method="spearman")

# Mitkä korrelaatiot ovat merkitseviä?
# Sosiaalinen ja emotionaalinen yksinäisyys, r=.31, p=.04
# Sosiaalinen yksinäisyys ja PNDL total, r=.82. p=.00
# Emotionaalinen yksinäisyys ja PNDL total, r=.76, p=.00
# EPCON ja EPATT, r=.45, p=.00
# EPPRO ja EPATT, r=.32, p=.04

# Lisää dikotominen sukupuoli korrelaatiomatriisiin:
tunteet$Gender <- factor(tunteet$Gender)
tunteet$Female <- 0
tunteet$Female[which(tunteet$Gender == "F")] <- 1

# Korrelaatiomatriisi, jossa mukana sukupuoli:
corr.test(tunteet[c(88:93,101)])

# Osittaiset korrelaatiot:
library(ppcor)
pcor.test(x = Social.loneliness, y = EPPRO, z = Emotional.loneliness)


