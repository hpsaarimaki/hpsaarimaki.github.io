# Kotitehtävät
# Viikko 7

# ---

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Kotitehtävät/')

# ---

# Aineiston valmistelu

# Ladataan aineisto:

greenspace <- read.csv('GREENSPACE.csv', sep=";")
greenspace$X <- NULL

# Tarkastellaan aineistoa:
summary(greenspace)

# Faktorit faktoreiksi:
greenspace$ID <- factor(greenspace$ID)
greenspace$Country <- factor(greenspace$Country)
greenspace$Gender <- factor(greenspace$Gender)

# Havaitaan, että joissakin muuttujissa on negatiivisia arvoja (-99).
# Nämä ovat todennäköisesti yritys koodata puuttuvia arvoja.
# Uudelleenkoodataan negatiiviset arvot NA:ksi.
greenspace[greenspace == -99] <- NA

# Poikkeavat havainnot:
# Luontomuuttujat:
boxplot(greenspace[5:7])
# CHAOS-muuttujat:
boxplot(greenspace[8:21])
# PCNB-muuttujat:
boxplot(greenspace[22:30])
# Wellbeing-muuttujat:
boxplot(greenspace[31:34])

# Ei tässä vaiheessa huolestuttavan poikkeavia havaintoja.

# Puuttuvien havaintojen määrä:
library(psych)
describe(greenspace)

# Huomataan, että joiltakin osallistujilta puuttuu osa muuttujista.
# Tallennetaan uuteen tietokehykseen vain osallistujat, joilta on koko aineisto (N=535):

greenspace <- greenspace[complete.cases(greenspace),]
describe(greenspace)
attach(greenspace)

# ---

# 2. Taustamuuttujien tarkastelu

# Tarkista iän keskiarvot asuinmaan suhteen:
tapply(Age, Country, mean)
tapply(Age, Country, sd)
tapply(Age, Country, range)

# Onko eroja iässä?
t.test(Age ~ Country)

# Tarkista asuinmaakohtainen sukupuolijakauma:
table(Gender, Country)

# Onko sukupuolijakauma sama eri asuinmaiden välillä?
chisq.test(Gender, Country)

# ---

# 3. Korrelaatiot

# Korrelaatiomatriisi selittävien ja selitettävien muuttujien välillä.
# Tarkista aineisto ja valitse oikea korrelaatiokerroin.

# Tarkista jakaumat:
par(mfrow=c(2,4))
hist(greenspace$NatConnect)# vino, diskreetti
hist(greenspace$NatEngage) # vino, diskreetti
hist(greenspace$NatTime)   # vino

hist(greenspace$CHAOS)
hist(greenspace$PCNB)
hist(greenspace$WELLBEING) # hieman vino


# Koska jakaumat ovat vinoja, käytetään Spearmanin korrelaatiokerrointa.

corr.test(greenspace[c(5:7,35:37)], method="spearman")

# ---

# 4. Lineaarinen regressio

# Luo malli, jossa ennustat hyvinvointia kaikilla luontomuuttujilla.
# Mitkä luontomuuttujista ennustavat hyvinvointia merkitsevästi?
# Mikä on mallin kokonaisselitysaste?

malli <- lm(WELLBEING ~ NatConnect + NatEngage + NatTime) 
plot(malli) # oletukset ok
library(car)
vif(malli) # multikollineaarisuus ok
summary(malli)

# Luontomuuttujista luontoyhteys (beta=0.52) ja vuorovaikutus luonnon kanssa (beta=0.70) ennustavat merkitsevästi
# hyvinvointia.
# Malli sopii aineistoon (F(3,531)=12.55, p<.0001) ja luontomuuttujat selittävät yhteensä 6% hyvinvoinnin
# vaihtelusta.



