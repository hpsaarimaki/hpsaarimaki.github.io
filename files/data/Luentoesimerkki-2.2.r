# PSY.204
# Luento 2.2
# T-testit
# Esimerkkiskripti
# HS 2.9.2024

# ---

# Asetetaan työskentelykansio:
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Harjoitukset')

# Aineiston lataaminen ja valmistelu:
source('Harjoitukset-R1.2.r')

# Kiinnitetään aineisto:
attach(naming)

# ---

# Aineiston kuvailu

# Keski- ja hajontaluvut koko tietokehykselle:
library(psych)
describe(naming)

# Keski- ja hajontaluvut erikseen eri ryhmille:
tapply(iq, list(sex, reading.group), mean, na.rm=T)	# keskiarvo (mean)
tapply(iq, list(sex, reading.group), sd, na.rm=T)	# keskihajonta (sd)



# ---

# Yhden otoksen t-testi

# Eroaako älykkyys otoksessamme populaation älykkyydestä?

# Oletusten tarkastelu:

# Jakaumat:

hist(iq)              # histogrammi
qqnorm(iq)            # qq-kuvio
qqline(iq, col="red")

# Normaalijakautuneisuustestit:

# Shapiro-Wilk
shapiro.test(iq)	

# Kolmogorov-Smirnov
ks.test(iq, pnorm, mean=mean(iq, na.rm=T), sd=sd(iq, na.rm=T))

# -

# Yhden otoksen t-testi:
t.test(iq, mu=100, data=naming)

# Efektikoko Cohenin D:
library(lsr)
cohensD(naming$iq, mu=100)

# ---

# Riippumattomien otosten t-testi:

# Oletusten tarkastelu:

# Histogrammit erikseen miehille ja naisille:
par(mfrow=c(1,2))
hist(regular, main='Miehet', xlab='Tavalliset sanat (ms)',
     data=naming_male)
hist(regular, main='Naiset', xlab='Tavalliset sanat (ms)',
     data=naming_female)

# Shapiro-Wilk
shapiro.test(naming_male$regular)
shapiro.test(naming_female$regular)

# Varianssien yhtäsuuruus
var.test(regular~sex, data=naming)

# -

# Riippumattomien otosten t-testi:
t.test(regular ~ sex, data=naming)

# Efektikoko:
library(lsr)
cohensD(regular ~ sex, data=naming)

# Keskiluvut ryhmittäin:
tapply(iq, list(sex, reading.group), mean, na.rm=T)	# keskiarvo (mean)
tapply(iq, list(sex, reading.group), sd, na.rm=T)	# keskihajonta (sd)

# Kuvio:
library(dplyr)
library(ggplot2)
naming %>%
  ggplot(aes(x = sex, y = regular, fill = sex)) +
  geom_boxplot() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  xlab("") +
  ylab("Tavalliset sanat (ms)") +  
  scale_x_discrete(labels = c("female" = "Naiset", "male" = "Miehet"))


# -

# Toistettujen mittausten t-testi

# Oletusten testaaminen:

# Histogrammit eri koetilanteille:
par(mfrow=c(1,2))
hist(regular, main="", xlab='Tavalliset sanat (ms)', ylab="")
hist(exception, main="", xlab='Epätavalliset sanat, (ms)', ylab="")

# Shapiro-Wilk eri koetilanteille:
shapiro.test(regular)
shapiro.test(exception)

# Varianssien yhtäsuuruus:
var.test(regular, exception)

# -

# Toistettujen mittausten t-testi
t.test(regular, exception, paired=T, data=naming)

# Efektikoko
cohensD(regular, exception, method="paired")

# Tulosten visualisointi laatikkokuvion avulla:
ggplot() +
  geom_boxplot(aes(x = "Tavalliset sanat", y = naming$regular), fill = "aquamarine") +
  geom_boxplot(aes(x = "Epätavalliset", y = naming$exception), fill = "chocolate") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  xlab("Sanan tyyppi") +
  ylab("Lukemiseen käytetty aika (ms)") +
  scale_x_discrete(labels = c("Tavallinen", "Epätavallinen"))

# ---

