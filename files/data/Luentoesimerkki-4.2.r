# PSY.204
# HS 18.9.2023
# Esimerkkiaineisto: TAS

# ---

# Asetetaan työskentelykansio:

setwd("C:/Users/sbhesa/Documents/Opetus/PSY.204")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto

tunteet <- read.csv('https://hpsaarimaki.github.io/file/data/tas.csv', sep=";")
colnames(tunteet)[1] <- "id"

# Tarkastellaan aineistoa

summary(tunteet)

# Muutetaan faktorit faktoreiksi

tunteet$id <- factor(tunteet$id)
tunteet$sukupuoli <- factor(tunteet$sukupuoli, levels=c(1,2), labels=c("mies", "nainen"))
tunteet$saately <- factor(tunteet$saately, levels=c(1,2,3), labels=c("valttely", "uudelleenarviointi", "kehollinen"))
tunteet$verkosto <- factor(tunteet$verkosto, levels=c(1,2), labels=c("kognitiivinen", "affektiivinen"))

# ---

# 2. Kuvailevat tulokset

# Tarkastellaan tunnuslukuja:
library(psych)
describe(tunteet)

# Visualisoidaan aineistoa:

par(mar=c(1,1,1,1)) # säädän marginaalien kokoa pienemmäksi
plot(tunteet) # sirontakuvio kaikkien muuttujien välillä

par(mar=c(1,1,1,1))
plot(tunteet[3:5]) # sirontakuvio kaikkien numeeristen muuttujien välillä

# Korrelaatiomatriisi numeeristen muuttujien välillä;
corr.test(tunteet[3:5]) 

# Etsitään vierashavainto:
which(tunteet$aleksitymia > 100)
which(tunteet$empatia > 100)

# Poistetaan vierashavainto ja tallennetaan uutena tietokehyksenä:
tunteet_valmis <- tunteet[-38,]

# Tarkastellaan uutta tietokehystä:
plot(tunteet_valmis[3:5])
corr.test(tunteet_valmis[3:5])

# Kiinnitetään lopullinen tietokehys:
detach()
attach(tunteet_valmis)


# Muita visuaalisia tarkasteluja:

# Ehdolliset sirontakuviot:

# Empatian ja aleksitymian suhde sukupuolen mukaan:
coplot(empatia ~ aleksitymia | sukupuoli)

# Toinen vaihtoehto äskeisen kuvion piirtämiseen:
library(ggplot2)
ggplot(tunteet_valmis, aes(x=aleksitymia, y=empatia)) +
  stat_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(~ sukupuoli) 
xyplot(empatia ~ aleksitymia | sukupuoli)

# ------------

# 3. Regressiomallin rakentaminen

# Yksi selittävä muuttuja (tässä aleksitymia):
malli1 <- lm(empatia ~ aleksitymia)
summary(malli1) # tarkastellaan mallia

# Kahden selittävän muuttujan päävaikutukset:
malli2 <- lm(empatia ~ aleksitymia + tunnesanat)

# Kahden selittävän muuttujan pää- ja yhdysvaikutukset:
malli3 <- lm(empatia ~ aleksitymia * tunnesanat)

# Polynomiaalinen eli käyräviivainen suhde:
tunteet_valmis$aleksitymia2 <- aleksitymia^2
malli4 <- lm(empatia ~ aleksitymia + aleksitymia2)

# ------------

# 4. Oletusten tarkastelu

# Näytetään diagnostiset kuviot mallille 1:
layout(matrix(c(1,2,3,4),2,2)) # säätää kuvan asettelua
plot(malli1)

# Cookin etäisyydet
influence.measures(malli1)

# Homoskedastisuus
library(car)
ncvTest(malli1)

# Jäännöstermien kuvaajat
library(ggResidpanel)
resid_panel(malli1)

# ----------

# 5. Tulosten tarkastelu

# Perustarkastelut mallille 1:
summary(malli1)
anova(malli1)

# Perustarkastelut mallille 2:
summary(malli2)

# Diagnostiset kuviot:
layout(matrix(c(1,2,3,4),2,2)) # säätää kuvan asettelua
plot(malli2)

# Multikolineaarisuus
cor.test(aleksitymia,tunnesanat)
library(car)
vif(malli2)

# Mallien vertailu
anova(malli1, malli2)

# --------
