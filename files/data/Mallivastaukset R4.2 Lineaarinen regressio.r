# PSY.204
# HS 23.9.2022
# Esimerkkiaineisto: Persoonallisuus

# ---

# Asetetaan ty�skentelykansio:
setwd("C:/Users/sbhesa/Documents/Opetus")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:

# Vaihtoehto 1: nummenmaa-kirjasto
library(nummenmaa)
perso <- persoonallisuus
summary(perso)

# Vaihtoehto 2: lataa aineisto netist�
perso <- read.csv("https://hpsaarimaki.github.io/files/data/persoonallisuus.csv")
perso[1] <- NULL
summary(perso)
perso$SUKUP <- factor(perso$SUKUP)
perso$KATISYYS <- factor(perso$KATISYYS)

# Kiinnitet��n aineisto
attach(perso)

# ---

# 2 Kuvailevat tulokset

# Tarkastellaan tunnuslukuja:
library(psych)
describe(perso)

# Sirontakuvio kaikkien numeeristen muuttujien v�lill�:
dev.new()
plot(perso[3:20]) 

# Sirontakuvio valikoiduille muuttujille:
dev.new()
plot(perso[c(3,7:9,15:19)])

# Korrelaatiomatriisi valikoiduille muuttujille:
corr.test(perso[c(3,7:9,15:19)])

# -

# Kysymys 1:
# Itsehillint� korreloi monivertailukorjauksen j�lkeen vain ahdistuneisuuden, stressin, ja neurotisismin kanssa.

# -

# Kysymys 2:
# Ahdistuneisuuden ja neurotisismin, masennuksen ja stressin korrelaatiot ovat yli .60.
# My�s masennus ja stressi (r = .63) ja stressi ja neurotisismi (r = .62) korreloivat.

# - 

# Kysymys 3:
dev.new()
coplot(ITSEHILLINTA ~ AHDISTUNEISUUS | SUKUP)
# Toinen vaihtoehto:
library(ggplot2)
ggplot(perso, aes(x=AHDISTUNEISUUS, y=ITSEHILLINTA)) +
  stat_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(~ SUKUP) 

# Itsehillint� laskee ahdistuneisuuden my�t� jyrkemmin miehill� kuin naisilla.

# -----

# 3 Lineaarinen regressio

# 3.1 Yhden selitt�v�n muuttujan mallit:

# Tallennetaan malli 1:

malli_1 <- lm(ITSEHILLINTA ~ TUNNOLLISUUS)

# -

# Kysymys 4:

# Tarkastellaan oletuksia:
plot(malli_1)
# Lineaarisuusoletus t�yttyy.
# Residuaalien normaalisuusoletus t�yttyy.
# Residuaalien homoskedastistuus on ok.
# Ei merkitt�vi� vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# -

# Kysymys 5:

# Tarkastellaan mallia:
summary(malli_1)
# Tunnollisuus ei ennusta itsehillint�� (F(1, 78)=0.001, p=.97).

# -

# Tallennetaan malli 2:
malli_2 <- lm(ITSEHILLINTA ~ NEUROTISISMI)

# -

# Kysymys 6:

# Tarkastellaan oletuksia:
plot(malli_2)
# Lineaarisuusoletus t�yttyy.
# Residuaalien normaalisuusoletus ok.
# Residuaalien homoskedastistuusoletus t�yttyy.
# Ei merkitt�vi� vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# - 

# Kysymys 7:

# Tarkastellaan mallia:
summary(malli_2)
# Neurotisismi ennustaa itsehillint�� (R^2 = 0.28, F(1, 78)=30.7, p<.001).
# Neurotisismi selitt�� siis 28% itsehillinn�n vaihtelusta.
# Kun neurotisismi kasvaa yhden pisteen, itsehillint� laskee 0.32 pistett�.

# -

# Kysymys 8:

# Julkaisukelpoinen kuva esim:
ggplot(perso, aes(x=NEUROTISISMI, y=ITSEHILLINTA)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

# ---

# 3.2 Kahden selitt�v�n muuttujan mallit:

# Tallennetaan malli 3:
malli_3 <- lm(ITSEHILLINTA ~ NEUROTISISMI + AHDISTUNEISUUS)

# -

# Kysymys 9:

# # Tarkastellaan oletuksia:
plot(malli_3)
# Lineaarisuusoletus t�yttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus t�yttyy.
# Ei merkitt�vi� vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
cor.test(NEUROTISISMI,AHDISTUNEISUUS)
library(car)
vif(malli_3)
# Neurotisismi ja ahdistuneisuus korreloivat voimakkaasti (r=.84, p<.0001).
# VIF-arvot silti hyv�ksytt�vien rajoissa.
# Huom. Tulkinnassa noudata varovaisuutta. Neurotisismi sy� osan
# ahdistuneisuuden vaihtelusta.

# -

# Kysymys 10:

# Tarkastellaan mallia:
summary(malli_3)
# Malli selitt�� 29% itsehillinn�n vaihtelusta (muokattu R^2 = 0.29, F(2, 77)=17.4, p<.001)
# Muuttujista vain neurotisismi ennustaa itsehillint�� (beta_1 = -0.21).

# -

# Kysymys 11:

# Mallien vertailu:
anova(malli_2, malli_3)
# Ahdistuneisuuden lis��minen ei paranna mallia. Se voidaan j�tt�� pois.
# Isommalla otoskoolla ahdistuneisuus olisi todenn�k�isesti parantanut mallia
# (t�m� n�hd��n merkitsev�� l�hestyv�st� p-arvosta).

# ---

# 3.3 Useamman selitt�v�n muuttujan mallit

# Tallennetaan malli 4:
malli_4 <- lm(ITSEHILLINTA ~ NEUROTISISMI + ULOSPAINSUUNT + AVOIMUUS + SOVINNOLLISUUS + TUNNOLLISUUS)

# -

# Kysymys 12:

# # Tarkastellaan oletuksia:
plot(malli_4)
# Lineaarisuusoletus t�yttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus t�yttyy.
# Ei merkitt�vi� vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
corr.test(perso[15:19])
library(car)
vif(malli_4)
# Ei voimakkaita korrelaatioita.
# Vain neurotisismi ja ulosp�insuuntautuneisuus korreloivat (r=-.33, p<.05).
# VIF-arvot hyv�ksytt�vien rajoissa.

# -

# Kysymys 13:

# Tarkastellaan mallia:
summary(malli_4)
# Malli selitt�� 29% itsehillinn�n vaihtelusta (muokattu R^2 = 0.27, F(5, 74)=6.84, p<.001)
# Muuttujista vain neurotisismi ennustaa itsehillint�� (beta_1 = -0.33).

# -

# Kysymys 14:

# Mallien vertailu:
anova(malli_2, malli_4)
# Muiden persoonallisuuspiirteiden lis��minen ei paranna mallia.
# Vain neurotisismi ennustaa itsehillint��.

# -

# Tallennetaan malli 5:
malli_5 <- lm(ITSEHILLINTA ~ NEUROTISISMI + MASENNUS + STRESSI)

# -

# Kysymys 15:
# Ahdistuneisuuden osalta todettiin aiemmin, ett� se ei merkitsev�sti ennusta itsehillint��.
# Siksi se voidaan j�tt�� jatkotarkasteluista pois.

# -

# Kysymys 16:

# # Tarkastellaan oletuksia:
plot(malli_5)
# Lineaarisuusoletus t�yttyy.
# Residuaalien normaalisuusoletus ok.
# Residuaalien homoskedastistuusoletus ok.
# Ei merkitt�vi� vierashavaintoja, mutta havainto 12 vet�� aineistoa hieman vinoon.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
corr.test(perso[c(8:9,15)])
library(car)
vif(malli_5)
# Kaikkien muuttujien v�liset korrelaatiot ovat melko voimakkaita.
# VIF-arvot silti hyv�ksytt�vien rajoissa.
# Huom. Neurotisismi selitt�� osan mielialamuuttujien vaihtelusta, 
# mik� t�ytyy huomioida tulosten tulkinnassa.

# -

# Kysymys 17:

# Tarkastellaan mallia:
summary(malli_5)
# Malli selitt�� 36% itsehillinn�n vaihtelusta (muokattu R^2 = 0.36, F(3, 76)=15.5, p<.001)
# Muuttujista neurotisismi ja stressi ennustavat itsehillint��.
# Kun neurotisismi kasvaa, itsehillint� laskee (beta_1 = -0.22).
# Kun stressi kasvaa, itsehillint� laskee (beta_1 = -0.52).

# -

# Kysymys 18:

# Mallien vertailu:
anova(malli_2, malli_5)
# Stressin ja masennuksen lis��minen malliin parantaa mallia.

# -

# Kysymys 19:
# Lopulliseen malliin voisi ottaa mukaan neurotisismin ja stressin.

# -----