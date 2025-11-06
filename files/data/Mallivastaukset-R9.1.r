# PSY.204
# HS 3.11.2025
# Esimerkkiaineisto: Persoonallisuus

# ---

# Asetetaan työskentelykansio:
setwd("C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2025-2026/PSY.204/Harjoitukset/")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:

perso <- read.csv("https://hpsaarimaki.github.io/files/data/persoonallisuus.csv")
summary(perso)

# Kategoriset muuttujat faktoreiksi:
perso$SUKUP <- factor(perso$SUKUP)
perso$KATISYYS <- factor(perso$KATISYYS)

# Kiinnitetään aineisto
attach(perso)

# ---

# 2 Kuvailevat tulokset

# Tarkastellaan tunnuslukuja:
library(psych)
describe(perso)

# Histogrammit:
hist(ITSEHILLINTA)
hist(NEUROTISISMI)
hist(ULOSPAINSUUNT)
hist(AVOIMUUS)
hist(SOVINNOLLISUUS)
hist(TUNNOLLISUUS)
hist(MASENNUS)
hist(AHDISTUNEISUUS)
hist(STRESSI)

# Sirontakuvio valikoiduille muuttujille:
dev.new()
plot(perso[c(4,8:10,16:20)])

# Korrelaatiomatriisi valikoiduille muuttujille:
corr.test(perso[c(4,8:10,16:20)])

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

# Tallennetaan malli 1:

malli_1 <- lm(ITSEHILLINTA ~ NEUROTISISMI + ULOSPAINSUUNT + AVOIMUUS + SOVINNOLLISUUS + TUNNOLLISUUS)

# -

# Kysymys 4:

# Tarkastellaan oletuksia:
plot(malli_1)
# Lineaarisuusoletus tÄyttyy.
# Residuaalien normaalisuusoletus tÄyttyy.
# Residuaalien homoskedastistuus on ok.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

library(car)
vif(malli_1)
# VIF-arvot hyväksyttävien rajoissa.


# -

# Kysymys 5:

# Tarkastellaan mallia:
summary(malli_1)
# Vain neurotisismi on yhteydessä itsehillintään.
# Malli sopii aineistoon (F(5,74)=6.84, p<.0001), selitysaste R^2=0.27.

# -

# Tallennetaan malli 2:
malli_2 <- lm(ITSEHILLINTA ~ NEUROTISISMI)

# ---

# Tallennetaan malli 3:
malli_3 <- lm(ITSEHILLINTA ~ NEUROTISISMI + AHDISTUNEISUUS + MASENNUS + STRESSI)

# -

# Kysymys 6:

# # Tarkastellaan oletuksia:
plot(malli_3)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
vif(malli_3)
# VIF-arvot hyväksyttävienrajoissa.
corr.test(perso[c(8:10,16)])
# Huom. Tulkinnassa noudata varovaisuutta. Korrelaatiot 0.49-0.74.

# -

# Kysymys 7:

# Tarkastellaan mallia:
summary(malli_3)
# Malli selittää 38% itsehillinnän vaihtelusta (muokattu R^2 = 0.38, F(4, 75)=12.9, p<.001)
# Muuttujista vain masennus ja stressi ennustavat itsehillintää.

# -

# Kysymys 8:

# Mallien vertailu:
anova(malli_2, malli_3)
# Mielialatekijöiden lisääminen parantaa mallia, selitysasteen muutos 0.11.

# ---

# Tallennetaan malli 4:
malli_4 <- lm(ITSEHILLINTA ~ NEUROTISISMI + AHDISTUNEISUUS + MASENNUS + STRESSI + SUKUP + AUTISMI)

# -

# Kysymys 9:

# # Tarkastellaan oletuksia:
plot(malli_4)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
vif(malli_4)
# VIF-arvot hyväksyttävien rajoissa.

# -

# Kysymys 10:

# Tarkastellaan mallia:
summary(malli_4)
# Malli selittää 39% itsehillinnän vaihtelusta (muokattu R^2 = 0.39, F(6, 73)=9.48, p<.001)
# Muuttujista vain masennus ja stressi ennustavat itsehillintää.

# -

# Kysymys 11:

# Mallien vertailu:
anova(malli_3, malli_4)
# Taustamuuttujien lisääminen ei paranna mallia.

# -

# Tallennetaan malli 5:
malli_5 <- lm(ITSEHILLINTA ~ (NEUROTISISMI + AHDISTUNEISUUS + MASENNUS + STRESSI) * SUKUP)

# -

# Kysymys 12:

# # Tarkastellaan oletuksia:
plot(malli_5)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok (muutama poikkeava havainto).
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos melko pieni (N=80).

# Multikollineaarisuus:
vif(malli_5, type='predictor')
# VIF-arvot hyväksyttävien rajoissa.

# -

# Kysymys 13:

# Tarkastellaan mallia:
summary(malli_5)
# Malli selittää 39% itsehillinnän vaihtelusta (muokattu R^2 = 0.39, F(9, 70)=6.58, p<.001)
# Muuttujista ahdistuneisuus ja sukupuoli ennustavat itsehillintää.
# Sukupuoli myös moderoi ahdistuneisuuden vaikutusta.

# Kuvio:
library(interactions)
interact_plot(malli_5, pred = AHDISTUNEISUUS, modx = SUKUP)


