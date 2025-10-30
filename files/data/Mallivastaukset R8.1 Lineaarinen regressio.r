# PSY.204 syksy 2025
# Mallivastaukset
# Harjoitusmoniste R8.1
# Heini Saarimäki

# ---

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2025-2026/PSY.204/Harjoitukset/')

# ---

# 1. Aineiston valmistelu

# Lataa aineisto:
tunteet <- read.csv('tunteet.csv')

# Tallenna vain analyysia varten tarvittavat muuttujat:
tunteet_valmis <- tunteet[c(2:5,89:100)]

# Kiinnitä aineisto
attach(tunteet_valmis)

# ---

# 2 Kuvailevat tulokset

# Tarkastellaan tunnuslukuja:
library(psych)
describe(tunteet_valmis[8:16])
# Huomioi taulukosta myös puuttuvan aineiston määrä per summamuuttuja.

# Sirontakuvio valikoiduille muuttujille:
dev.new() # avaa kuvan erilliseen ikkunaan
plot(tunteet_valmis[8:16]) 

# Korrelaatiomatriisi valikoiduille muuttujille:
corr.test(tunteet_valmis[8:16])

# -

# Kysymys 1:
# Empatian alaskaalojen ja tunteiden tunnistamisen alaskaalojen välillä on seuraavat
# tilastollisesti merkitsevät korrelaatiot:
# EPCON ja ABOD (r=-.51)
# EPATT ja AOTH (r=.67)
# EPATT ja AOWN (r=.52)
# EPPRO ja AOTH (r=.44)

# Huomioi pieni otoskoko, jolloin kaikki kohtalaisetkaan korrelaatiot eivät ole
# monivertailukorjauksen jälkeen merkitseviä.

# -

# Kysymys 2:
# Selittäviä muuttujia ovat tunteiden tunnistamisen alaskaalat. Niiden välillä on seuraavat
# tilastollisesti merkitsevät korrelaatiot:
# ADIF ja ABOD (r=.41)
# ATALK ja AHIDE (r=.47)

# -----

# 3 Lineaarinen regressio

# 3.1 Yhden selittävän muuttujan mallit:

# Tallennetaan malli 1:

malli_1 <- lm(EPCON ~ ADIF)

# -

# Kysymys 4:

# Tarkastellaan oletuksia:
plot(malli_1)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus täyttyy.
# Residuaalien homoskedastistuus on ok.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos pieni (N=57).

# -

# Kysymys 5:

# Tarkastellaan mallia:
summary(malli_1)
# Tunteiden erittely ei ennusta tunteiden tarttumista (mallin sopivuus: F(1, 53)=2.34, p=.132).

# -

# Tallennetaan malli 2:
malli_2 <- lm(EPCON ~ ABOD)

# -

# Kysymys 6:

# Tarkastellaan oletuksia:
plot(malli_2)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok.
# Residuaalien homoskedastistuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos pieni (N=57).

# - 

# Kysymys 7:

# Tarkastellaan mallia:
summary(malli_2)
# Tunteiden kehollinen tunnistaminen selittää tunteiden tarttumista (R^2 = 0.26, F(1, 53)=18.2, p<.001).
# Tunteiden kehollinen tunnistaminen selittää siis 26% tunteiden tarttumisen vaihtelusta.
# Kun tunteiden kehollinen tunnistaminen kasvaa yhden pisteen, tunteiden tarttuminen laskee 0.31 pistettä.

# -

# Kysymys 8:

# Julkaisukelpoinen kuva esim:
ggplot(tunteet_valmis, aes(x=EPCON, y=ABOD)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

# ---

# 3.2 Kahden selittävän muuttujan malli:

# Tallennetaan malli 3:
malli_3 <- lm(EPCON ~ ABOD + AOTH)

# -

# Kysymys 9:

# # Tarkastellaan oletuksia:
plot(malli_3)
# Lineaarisuusoletus täyttyy.
# Residuaalien normaalisuusoletus ok.
# Residuaalien homoskedastisuusoletus täyttyy.
# Ei merkittäviä vierashavaintoja.
# Huom. Otos pieni (N=57).

# Multikollineaarisuus:
cor.test(ABOD,AOTH)
library(car)
vif(malli_3)
# Tunteiden kehollinen tunnistainen ja tarkkaavuus muiden tunteille korreloivat jonkin verran (r=.-27, p=.04),
# mutta korrelaatio ei ole kovin voimakasta.
# VIF-arvot ok (arvot välillä 1-5 ok).

# -

# Kysymys 10:

# Tarkastellaan mallia:
summary(malli_3)
# Malli selittää 31% tunteiden tarttumisen vaihtelusta (muokattu R^2 = 0.31, F(2, 52)=13.2, p<.001)
# Tunteiden tarttuminen laskee, kun tunteiden kehollinen tunnistaminen kasvaa (beta=-0.26).
# Tunteiden tarttuminen kasvaa, kun tarkkaavuus toisten tunteille kasvaa (beta=0.25).

# -

# Kysymys 11:

# Mallien vertailu:
anova(malli_2, malli_3)
# Tarkkaavuus toisten tunteille parantaa mallia.

# ---

