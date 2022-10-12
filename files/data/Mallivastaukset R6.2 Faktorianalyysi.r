# PSY204 syksy 2022
# Harjoitusmoniste 6.2: Faktorianalyysi
# Mallivastaukset
# Heini Saarim�ki 9.10.2022
# 
# ----

# Ladataan tarvittavat kirjastot
library(psych)
library(GPArotation)

# Asetetaan ty�skentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ----

# 1. Aineiston valmistelu

# Ladataan aineisto:

tas <- read.csv("2022-2023/PSY204 - syksy 2022/Materiaalit/briganti-tas-data.csv", sep=";")

# Tarkistetaan puuttuvat havainnot:
describe(tas)
anyNA(tas) 

# ei puuttuvia havaintoja, N = 1925

# Tarkastellaan muuttujien jakaumia:
par(mfrow=c(1,5))
hist(tas$TAS1)
hist(tas$TAS2)
hist(tas$TAS3)
hist(tas$TAS4)
hist(tas$TAS5)

# Tarkastellaan korrelaatiomatriisia visuaalisesti:
library(GGally)
ggcorr(tas) 

# -

# Aineiston faktoroitumisen arviointi:

# Kaksi keinoa:

# 1) Bartlettin sf��risyystesti

cortest.bartlett(tas)

# Merkitsev� tulos (p<.05), eli ainakin jotkin muuttujat korreloivat kesken��n.

# 2) KMO

KMO(tas)

# Kokonais-MSA = 0.84, voidaan jatkaa faktorianalyysiin.

# ---

# 2. Faktorimallin suunnittelu

# Faktorien m��r�n valinta

# Kaksi menetelm��:

# 1) Scree-kuvio

scree(tas)

# arviolta 2-3 faktoria

# 2) Parallel analysis

fa.parallel(tas)

# arviolta 4 faktoria, joten aloitetaan siit�

# ---

# 3. Faktorianalyysin toteutus

# Kokeillaan nelj�n faktorin ratkaisua:

fa4 <- fa(tas,
          nfactor=4,
          fm="pa",
          max.iter=100,
          rotate="oblimin")

fa.diagram(fa4)
fa4

# Jatketaan nelj�n faktorin mallin tarkasteluilla.

# ---

# 4. Mallin tulkinta

# Yleistulokset mallista:

fa4

# Faktoriratkaisun sopivuus:
# khii2(116)=589.6, p<.0001
# TLI = 0.891 --> alle 0.9 olisi ok, eli ei hyv�
# RMSEA = 0.046 --> alle 0.05 on ok

# Lataukset kullekin faktorille:
library(plot.matrix)
plot(loadings(fa4), cex=0.5, axis.row = list(side=2, las=1))
# Kaikki muuttujat latautuvat v�hint��n .3 jollekin faktorille,
# mutta osalla muuttujista lataukset pieni� (alle .4).

# Muuttujien kommunaliteetit erikseen:
fa4$communality
# Alle .3 kommunaliteetit:
# TAS3, TAS5, TAS 6, TAS8, TAS10, TAS13, TAS10,
# TAS11, TAS14, TAS16, TAS18, TAS19, TAS20

# Faktorien selitysosuus:
# Yksitt�isten faktorien selitysosuus 4-12%
# Kokonaisselitysosuus 31%

# Faktoreiden v�liset korrelaatiot
# kohtalainen (r=0.51) vain faktoreille 1 ja 3
# vinorotaatio perusteltu

# Faktoreiden nime�minen
# PA1: vaikeus kuvailla tunteita
# PA2: taipumus ongelmanratkaisuun
# PA3: vaikeus tunnistaa tunteita tai niiden syit� 
# PA4: haluttomuus jakaa tunteita

# Yhteenvetona:
# Lataukset ja kommunaliteetit osalle muuttujista pieni�.
# Kokeillaan kolmen faktorin ratkaisua.

# ---

# 5. Faktorianalyysin muokkaus: kolmen faktorin ratkaisu

# Ajetaan kolmen faktorin malli:
fa3 <- fa(tas,
          nfactor=3,
          fm="pa",
          max.iter=100,
          rotate="oblimin")

# Visuaalinen tarkastelu:
fa.diagram(fa3)

# ---

# 6. Mallin tulkinta: kolmen faktorin ratkaisu

# Yleistulokset mallista:

fa3

# Faktoriratkaisun sopivuus:
# khii2(133)=969.08, p<.0001
# TLI = 0.83 eli ei kovin hyv�
# RMSEA = 0.057 eli kohtalainen

# Lataukset kullekin faktorille:
library(plot.matrix)
plot(loadings(fa3), cex=0.5, axis.row = list(side=2, las=1))
# Kaikki muuttujat latautuvat v�hint��n .3 jollekin faktorille,
# lataukset parempia kuin nelj�n faktorin mallissa.

# Muuttujien kommunaliteetit erikseen:
fa3$communality
# Alle .3 kommunaliteetit:
# TAS3, TAS5, TAS 6, TAS7, TAS8, TAS10, 
# TAS11, TAS12, TAS14, TAS15, TAS16, TAS18, TAS19, TAS20

# Faktorien selitysosuus:
# Yksitt�isten faktorien selitysosuus 6-12%
# Kokonaisselitysosuus 28%

# Faktoreiden v�liset korrelaatiot
# kohtalainen (r=0.56) vain faktoreille 1 ja 3
# vinorotaatio perusteltu

# Faktoreiden nime�minen
# PA1: vaikeus puhua tunteista
# PA2: vaikeus tunnistaa tunteita
# PA3: taipumus ulkoisesti suuntautuneeseen ajatteluun


# ---


