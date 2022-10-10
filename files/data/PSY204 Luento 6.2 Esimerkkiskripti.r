# PSY204 syksy 2022
# Luento 6.1: Faktorianalyysi
# Esimerkkiskripti
# Heini Saarimäki 3.10.2022
# 
# ----

# Ladataan tarvittavat kirjastot
library(psych)
library(GPArotation)

# Asetetaan työskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ----

# 1. Aineiston valmistelu

# Ladataan aineisto:

bfi <- bfi
names(bfi)
summary(bfi)

# Muutetaan faktorit faktoreiksi
bfi$gender <- factor(bfi$gender)
bfi$education <- factor(bfi$education)

# Arvioidaan, kuinka paljon aineistoa puuttuu:
describe(bfi)
sum(complete.cases(bfi[1:25]))
dim(bfi)

# Puuttuvia arvoja on n. 16%, mutta aineistoa on silti riittävästi
# faktorianalyysia varten.

# Tarkastellaan muuttujien jakaumia:
par(mfrow=c(1,5))
hist(bfi$A1)
hist(bfi$A2)
hist(bfi$A3)
hist(bfi$A4)
hist(bfi$A5)

# Tarkastellaan korrelaatiomatriisia visuaalisesti:
library(GGally)
ggcorr(bfi[1:25]) 

# -

# Aineiston faktoroitumisen arviointi:
# Onko aineistossa löydettävissä merkityksellisiä latentteja faktoreita?
# Eli sopiiko faktorianalyysi aineistolle ylipäätään?

# Kaksi keinoa:

# 1) Bartlettin sfäärisyystesti

cortest.bartlett(bfi[1:25])

# Merkitsevä tulos (p<.05), eli ainakin jotkin muuttujat korreloivat keskenään.

# 2) KMO

KMO(bfi[1:25])

# Kokonais-MSA = 0.85, voidaan jatkaa faktorianalyysiin.

# ---

# 2. Faktorimallin suunnittelu

# Faktorien määrän valinta

# Kaksi menetelmää:

# 1) Scree-kuvio

scree(bfi[,1:25])

# 2) Parallel analysis

fa.parallel(bfi[1:25])

# Kuudes faktori lähellä rajaa, joten ehkä halutaan
# kokeilla sekä 5 ja 6 faktorin ratkaisuja.

# ---

# 3. Faktorianalyysin toteutus

# Kokeillaan kuuden faktorin ratkaisua:

pa6 <- fa(bfi[1:25],
          nfactor=6,
          fm="pa",
          max.iter=100,
          rotate="oblimin")

# Tarkastellaan kuviota:

fa.diagram(pa6)

# Faktorissa 6 on vain yksi muuttuja, luultavasti
# malli siis ylisovitettu.

# -

# Kokeillaan viiden faktorin ratkaisua:

pa5 <- fa(bfi[1:25],
          nfactor=5,
          fm="pa",
          max.iter=100,
          rotate="oblimin")

# Tarkastellaan kuviota:

fa.diagram(pa5)

# Ratkaisu on parempi, joten jatketaan sen tarkasteluilla.

# ---

# 4. Mallin tulkinta

# Yleistulokset mallista:

pa5

# Kommunaliteetit erikseen:

pa5$communality

# Rotatoitu ratkaisun latausmatriisi erikseen:

print(pa5$loadings, cutoff=0, digits=3)

# --

# 5. Tulosten raportointi:

# Latausten kuvaaja sopivin akselein

lataukset <- pa5$loadings[,1:2]

plot(lataukset, ylim=c(-1,1), xlim=c(-1,1))
text(lataukset, labels=names(bfi[1:25]),
     cex = .8, col="blue", pos=4)
abline(h = 0, v=0)

# ---