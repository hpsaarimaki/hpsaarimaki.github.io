# PSY.204
# HS 23.9.2022
# Esimerkkiaineisto: Persoonallisuus

# ---

# Asetetaan työskentelykansio:
setwd("C:/Users/sbhesa/Documents/Opetus")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:

# Vaihtoehto 1: nummenmaa-kirjasto
library(nummenmaa)
perso <- persoonallisuus
summary(perso)

# Vaihtoehto 2: lataa aineisto netistä
perso <- read.csv("https://hpsaarimaki.github.io/files/data/persoonallisuus.csv")
perso[1] <- NULL
summary(perso)
perso$SUKUP <- factor(perso$SUKUP)
perso$KATISYYS <- factor(perso$KATISYYS)

# Kiinnitetään aineisto
attach(perso)

# ---

# 2 Kuvailevat tulokset

# Tarkastellaan tunnuslukuja:
library(psych)
describe(perso)

# Sirontakuvio kaikkien numeeristen muuttujien välillä:
dev.new()
plot(perso[3:20]) 

# Sirontakuvio valikoiduille muuttujille:
dev.new()
plot(perso[c(3,7:9,15:19)])

# Korrelaatiomatriisi valikoiduille muuttujille:
corr.test(perso[c(3,7:9,15:19)])

# -

# Kysymys 1:
# Itsehillintä korreloi monivertailukorjauksen jälkeen vain ahdistuneisuuden, stressin, ja neurotisismin kanssa.

# -

# Kysymys 2:
# Ahdistuneisuuden ja neurotisismin, masennuksen ja stressin korrelaatiot ovat yli .60.
# Myös masennus ja stressi (r = .63) ja stressi ja neurotisismi (r = .62) korreloivat.

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

# Itsehillintä laskee ahdistuneisuuden myötä jyrkemmin miehillä kuin naisilla.

# -----

# 3 Lineaarinen regressio

# 3.1 Yhden selittävän muuttujan mallit:

# Tallennetaan malli 1:

malli_1 <- lm(ITSEHILLINTA ~ TUNNOLLISUUS)

# -

# Kysymys 4:

# Tarkastellaan oletuksia:
plot(malli_1)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus täyttyy.
# Residuaalien homoskedastistuus on ok.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# -

# Kysymys 5:

# Tarkastellaan mallia:
summary(malli_1)
# Tunnollisuus ei ennusta itsehillintää (F(1, 78)=0.001, p=.97).

# -

# Tallennetaan malli 2:
malli_2 <- lm(ITSEHILLINTA ~ NEUROTISISMI)

# -

# Kysymys 6:

# Tarkastellaan oletuksia:
plot(malli_2)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok.
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# - 

# Kysymys 7:

# Tarkastellaan mallia:
summary(malli_2)
# Neurotisismi ennustaa itsehillintää (R^2 = 0.28, F(1, 78)=30.7, p<.001).
# Neurotisismi selittää siis 28% itsehillinnän vaihtelusta.
# Kun neurotisismi kasvaa yhden pisteen, itsehillintä laskee 0.32 pistettä.

# -

# Kysymys 8:

# Julkaisukelpoinen kuva esim:
ggplot(perso, aes(x=NEUROTISISMI, y=ITSEHILLINTA)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

# ---

# 3.2 Kahden selittävän muuttujan mallit:

# Tallennetaan malli 3:
malli_3 <- lm(ITSEHILLINTA ~ NEUROTISISMI + AHDISTUNEISUUS)

# -

# Kysymys 9:

# # Tarkastellaan oletuksia:
plot(malli_3)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
cor.test(NEUROTISISMI,AHDISTUNEISUUS)
library(car)
vif(malli_3)
# Neurotisismi ja ahdistuneisuus korreloivat voimakkaasti (r=.84, p<.0001).
# VIF-arvot silti hyväksyttävien rajoissa.
# Huom. Tulkinnassa noudata varovaisuutta. Neurotisismi syö osan
# ahdistuneisuuden vaihtelusta.

# -

# Kysymys 10:

# Tarkastellaan mallia:
summary(malli_3)
# Malli selittää 29% itsehillinnän vaihtelusta (muokattu R^2 = 0.29, F(2, 77)=17.4, p<.001)
# Muuttujista vain neurotisismi ennustaa itsehillintää (beta_1 = -0.21).

# -

# Kysymys 11:

# Mallien vertailu:
anova(malli_2, malli_3)
# Ahdistuneisuuden lisääminen ei paranna mallia. Se voidaan jättää pois.
# Isommalla otoskoolla ahdistuneisuus olisi todennäköisesti parantanut mallia
# (tämä nähdään merkitsevää lähestyvästä p-arvosta).

# ---

# 3.3 Useamman selittävän muuttujan mallit

# Tallennetaan malli 4:
malli_4 <- lm(ITSEHILLINTA ~ NEUROTISISMI + ULOSPAINSUUNT + AVOIMUUS + SOVINNOLLISUUS + TUNNOLLISUUS)

# -

# Kysymys 12:

# # Tarkastellaan oletuksia:
plot(malli_4)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
corr.test(perso[15:19])
library(car)
vif(malli_4)
# Ei voimakkaita korrelaatioita.
# Vain neurotisismi ja ulospäinsuuntautuneisuus korreloivat (r=-.33, p<.05).
# VIF-arvot hyväksyttävien rajoissa.

# -

# Kysymys 13:

# Tarkastellaan mallia:
summary(malli_4)
# Malli selittää 29% itsehillinnän vaihtelusta (muokattu R^2 = 0.27, F(5, 74)=6.84, p<.001)
# Muuttujista vain neurotisismi ennustaa itsehillintää (beta_1 = -0.33).

# -

# Kysymys 14:

# Mallien vertailu:
anova(malli_2, malli_4)
# Muiden persoonallisuuspiirteiden lisääminen ei paranna mallia.
# Vain neurotisismi ennustaa itsehillintää.

# -

# Tallennetaan malli 5:
malli_5 <- lm(ITSEHILLINTA ~ NEUROTISISMI + MASENNUS + STRESSI)

# -

# Kysymys 15:
# Ahdistuneisuuden osalta todettiin aiemmin, että se ei merkitsevästi ennusta itsehillintää.
# Siksi se voidaan jättää jatkotarkasteluista pois.

# -

# Kysymys 16:

# # Tarkastellaan oletuksia:
plot(malli_5)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok.
# Residuaalien homoskedastistuusoletus ok.
# Ei merkittäviä vierashavaintoja, mutta havainto 12 vetää aineistoa hieman vinoon.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
corr.test(perso[c(8:9,15)])
library(car)
vif(malli_5)
# Kaikkien muuttujien väliset korrelaatiot ovat melko voimakkaita.
# VIF-arvot silti hyväksyttävien rajoissa.
# Huom. Neurotisismi selittää osan mielialamuuttujien vaihtelusta, 
# mikä täytyy huomioida tulosten tulkinnassa.

# -

# Kysymys 17:

# Tarkastellaan mallia:
summary(malli_5)
# Malli selittää 36% itsehillinnän vaihtelusta (muokattu R^2 = 0.36, F(3, 76)=15.5, p<.001)
# Muuttujista neurotisismi ja stressi ennustavat itsehillintää.
# Kun neurotisismi kasvaa, itsehillintä laskee (beta_1 = -0.22).
# Kun stressi kasvaa, itsehillintä laskee (beta_1 = -0.52).

# -

# Kysymys 18:

# Mallien vertailu:
anova(malli_2, malli_5)
# Stressin ja masennuksen lisääminen malliin parantaa mallia.

# -

# Kysymys 19:
# Lopulliseen malliin voisi ottaa mukaan neurotisismin ja stressin.

# -----