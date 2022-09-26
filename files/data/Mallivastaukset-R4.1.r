# PSY204
# Harjoitusmoniste R4.1
# Heini Saarimäki 21.9.2022

# ---

# 1. Aineiston valmistelu

# Asetetaan työskentelykansio:
setwd('C:/Users/sbhesa/Documents/Opetus')

# Ladataan aineisto:
data <- read.table('https://hpsaarimaki.github.io/files/data/students.txt', header=T, sep="",na.strings="NA")

# Tarkastellaan aineistoa:
head(data)
summary(data)

# Kysymys 1:
# Extra-muuttujassa on paljon puuttuvia arvoja, jotka on valmiiksi koodattu NA:lla.
# Stress-muuttujan maksimiarvo on epäilyttävän korkea.
# Tutkitaan Stress-muuttujaa tarkemmin laatikkokuviolla:
boxplot(data$Stress)
# Huomataan, että arvo 999 poikkeaa muista arvoista huomattavasti.
# Etsitään, millä rivillä arvo 999 datassa on:
which(data$Stress == 999)
# Arvo 999 löytyy viideltä riviltä.

# Kysymys 2:
# Luodaan uusi tietokehys:
data2 <- data
# Tehdään poikkeavien arvojen korjaus vain uudelle tietokehykselle data2.
# Muutetaan poikkeavat arvot R:n ymmärtämiksi puuttuviksi arvoiksi (NA):
data2[which(data2$Stress == 999),7] <- NA # huom. Stress-muuttuja sarakkeessa 7
# Tarkastellaan muutoksen vaikutuksia:
summary(data2)
boxplot(data2$Stress)
# Hyvältä näyttää!

# Kysymys 3:
# Voit tarvittaessa vertailla tekemiesi muutosten (esim. datamuunnokset)
# vaikutusta analyysiesi tuloksiin.

# Kiinnitetään muokattu tietokehys:
detach() # irrotetaan mahdolliset aiemmat tietokehykset
attach(data2)

# ---

# 2. Kuvailevat tulokset

# Tulostetaan tunnusluvut:
library(psych)
describe(data2)

# Kysymys 4:
# Muuttuja "Entry" on mitattu vain 37 osallistujalta.

# Kysymys 5:

# Tulostetaan histogrammit:
hist(Performance)
hist(Hours)
hist(Educ)
hist(Rating)
hist(Entry)
hist(Extra)
hist(Stress)

# Huomataan, että muuttujan Extra jakauma on vino.
# Lisäksi huomataan, että muuttuja Educ on diskreetti. Opiskeluvuosien määrä on mitattu kokonaisina vuosina.

# Kysymys 6:

# Tarkastellaan sirontakuvioita:
plot(Performance,Hours)
plot(data2)

# Loppukokeen arvosanan ja poissaolotuntien välillä näyttää olevan negatiivinen korrelaatio.
# Loppukokeen arvosanan, kurssiarvioinnin ja stressin välillä näyttää olevan positiivinen korrelaatio.
# Loppukokeen arvosanan ja alkutestin välillä ei näytä olevan voimakasta korrelaatiota.
# Opiskeluvuosien diskreetti asteikko tekee korrelaatioiden tulkinnasta hankalaa.
# Lisäharjoitusten vaikutusta on vaikea tulkita, koska aineistoa on niin vähän.

# ---

# 2. Korrelaatiot

# 2.1 Pearson

# Kahden muuttujan välinen korrelaatio:
cor.test(Performance, Hours)

# Kahden muuttujan välinen sirontakuvio:
plot(Performance, Hours)

# Koko aineiston korrelaatiomatriisi:
corr.test(data2)

# Kysymys 7:
# Seuraavat korrelaatiot eivät ole merkitseviä monivertailukorjausten jälkeen:
# Loppukokeen arvosana ja opiskeluvuodet, alkutestin tulos, lisätehtävien määrä ja stressi eivät korreloi.
# Poissaolotunnit ja opiskeluvuosien lukumäärä, alkutestin arvosana sekä lisätehtävien määrä eivät korreloi.
# Opiskeluvuosien lukumäärä korreloi vain poissaolotuntien kanssa.
# Arviointi ja alkutestin tulos, lisätehtävien määrä ja stressi eivät korreloi.
# Alkutesti korreloi vain poissaolotuntien ja opiskeluvuosien kanssa.
# Lisätehtävät eivät korreloi minkään muuttujien kanssa.
# Stressitaso korreloi vain poissaolotuntien kanssa.

# Kysymys 8:
# Maksimiotoskoko on 228, minimi 37.
# Minimi koskee lisätuntimuuttujaa, jossa oli vain vähän havaintoja.
# Lisätunnit ei ole luotettava muuttuja, koska havaintoja on vain pieneltä osajoukolta.

# Kysymys 9:
# Ei päde. Opiskeluvuodet, alkutestin tulos ja lisätehtävät eivät vaikuta suoritustasoon.

# Kysymys 10:
# Eivät ole.
# Poissaolotuntien lukumäärä korreloi negatiivisesti muiden muuttujien kanssa.
# Samoin lisätehtävien määrä korreloi negatiivisesti suoritustason, poissaolotuntien ja kurssin arvioinnin kanssa.
# Lisätehtävien määrä ei kuitenkaan ole luotettava muuttuja, koska sillä on paljon puuttuvia arvoja.
# Emme tiedä tarkasti, miksi tällä muuttujalla on paljon puuttuvia arvoja.

# -

# 2.2 Spearman

# Spearmanin korrelaatio kurssin loppuarvosanan ja poissaolotuntien välillä:
cor.test(Performance, Hours, method="spearman")

# Testi antaa varoituksen, koska Hours-muuttujassa on useampia yhtä suuria arvoja.
# Siksi järjestyksen tarkka laskeminen ei onnistu. R ei huomioi yhtä suuria arvoja.

# Varoituksen voi välttää näin:
cor.test(Performance, Hours, method="spearman", exact=FALSE)

# Huomataan, että tulokset ovat samat.

# Kysymys 10:
corr.test(data2, method="spearman")

# Kysymys 11:
# Tallenna korrelaatiomatriisit:
pearson <- corr.test(data2[c(1:2,4:7)])
spearman <- corr.test(data2[c(1:2,4:7)], method="spearman")
# Tutki objektin rakennetta:
str(pearson)
# Näytä esim. p-arvot molemmille objekteille:
pearson$p
# ...liikaa desimaaleja, kokeillaan sen sijaan:
round(pearson$p,2)
round(spearman$p,2)
# Testit antavat muuten samat tulokset, mutta stressin ja suoriutumisen yhteys ei ole merkitsevä 
# Spearmanin korrelaatiokerrointa käytettäessä.

# -

# 2.3 Osittaiset korrelaatiot

# Asenna tarvittaessa: install.packages('ppcor')
library(ppcor)
pcor.test(Performance, Entry, Hours)

# Huomataan, että suoritustaso ja alkutesti eivät korreloi merkitsevästi,
# kun poissaolotuntien vaikutus on huomioitu (p=-.05, p=.449).

# ---

# 3. Regressio

# 3.1 Yksinkertaisen mallin ajaminen

# Tallennetaan ensimmäinen malli:
model1 <- lm(Performance ~ Hours)

# Tutkitaan mallia:
summary(model1)

# -

# 3.2 Oletusten arviointi

# Normaalijakautuneisuus:
layout(matrix(c(1,2,3,4),2,2)) # säätää kuvan asettelua
plot(model1)

# Lisäksi residuaalien histogrammit:
library(ggResidpanel)
resid_panel(model1)

# Homoskedastisuus:
library(car)
ncvTest(model1)

# Vaikuttavat havainnot:
influence.measures(model1)

# -

# 3.3 Useamman muuttujan regressio

# Luodaan malli:
model2 <- lm(Performance ~ Hours + Entry)

# Oletusten tarkastelu:

# Normaalijakautuneisuus:
layout(matrix(c(1,2,3,4),2,2)) # säätää kuvan asettelua
plot(model2)

# Lisäksi residuaalien histogrammit:
resid_panel(model2)

# Homoskedastisuus:
ncvTest(model2)

# Vaikuttavat havainnot:
influence.measures(model2)

# Multikollineaarisuus:
cor.test(Hours, Entry)
library(car)
vif(model2)

# Kysymys 12:
summary(model2)
# Alkutestin pistemäärä ei ole merkitsevä selittäjä.

# Kysymys 13:
anova(model1, model2)
# Toinen malli ei ole ensimmäistä parempi.

# Kysymys 14:
# Poistetaan alkutesti eli "Entry" mallista.

# Kysymys 15:
# Kokeillaan ajaa uusi malli:
model3 <- lm(Performance ~ Hours + Extra)
summary(model3)
# Summary kertoo, että mallista poistettiin 191 havaintoa,
# koska ne puuttuvat muuttujasta Extra.
# Malli on siis muodostettu varsin pienellä osajoukolla koko aineistosta
# eikä siksi ole luotettava.

# Kysymys 16:
model4 <- lm(Performance ~ Hours + Rating)

# Kysymys 17:
summary(model4)
# Rating on merkitsevä selittäjä.
 
# Kysymys 18:
anova(model1,model4)
# Malli 4 on merkitsevästi parempi kuin ensimmäinen.

# Kysymys 19:
# Adjusted R^2 eli muokattu R^2.

# Kysymys 20:
# 12% varianssista.

# -

# 3.4 Yhdysvaikutuksen lisääminen malliin

# Luodaan malli:
model5 <- lm(Performance ~ Hours * Rating)
summary(model5)

# Kysymys 21:
# Päävaikutukset.

# Kysymys 22:
cor.test(Hours,Rating)
# Ovat, niiden välinen korrelaatio on r=-.26, p<.001.

# Kysymys 23:
anova(model4,model5)
# Yhdysvaikutuksen lisääminen ei paranna mallia.

# -

# 3.5 Polynomiaaliset yhteydet

# Lisätään uusi muuttuja Rating2:
data2$Rating2 <- Rating^2
attach(data2)

# Kysymys 24:
# y ~ x + x^2

# Ajetaan uusi malli:
model6 <- lm(Performance ~ Rating + Rating2)

# Kysymys 25:
summary(model6)
# Rating2 ei ennusta suoritustasoa
# Ratingin ja suoritustason yhteys ei ole polynomiaalinen

# Ajetaan uusi malli:
model7 <- lm(Performance ~ Hours*Educ*Rating*Entry*Stress)
# Jätettiin pois Extra, jossa oli vähän havaintoja

# Kysymys 26:
summary(model7)
# R^2-arvo on suurempi, muokattu R^2-arvo pienempi 

# Kysymys 27:
# Malli 7 on monimutkaisempi, mikä huomioidaan
# muokattua selitysastetta laskiessa.

# ---

# 4. Lisämateriaalia: kontrastit

# 4.1 Kontrastien rakentaminen

# Muutetaan Education kategoriseksi muuttujaksi:
data2$Educ <- factor(data2$Educ)
attach(data2)

# Tarkistetaan kategorisen muuttujan tasot:
levels(Educ)

# Määritetään kontrastit:
contrasts(Educ) <- cbind(c(-0.5,-0.5,1),c(1,-1,0))

# Tarkistetaan kontrastit:
contrasts(Educ)

# 4.2 Kontrastien ajaminen

# Tallennetaan malli ja tarkastellaan sitä:
M1 <- lm(Performance ~ Educ)
summary(M1)

