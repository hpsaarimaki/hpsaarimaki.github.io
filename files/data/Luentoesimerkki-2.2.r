# PSY.204
# HS 20.6.2022
# Esimerkkiaineisto: Blindspots and spotlights

# ---

# Asetetaan työskentelykansio:

setwd("C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/R-harkat")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto

data <- read.csv('https://hpsaarimaki.github.io/files/data/scotoma.csv')
head(data)

# Huomioi, että aineisto on pitkässä muodossa (eri koetilanteet eri riveillä).
# Tämä on hyvä, koska tilastollisen testin ajava funktio (aov_car()) edellyttää pitkää muotoa.

# Tarkastellaan aineistoa:
summary(data)

# Muutetaan muuttuja 'subject' faktoriksi:

data$subject <- as.factor(data$subject)

# Puuttuvat arvot: puuttuvia arvoja ei näytä olevan

# Kiinnitetään tietokehys:

attach(data)

# ---

# 2. Kuvailevat tulokset

# Keski- ja hajontaluvut:
library(psych)
describe(data)

# Esim. keskiarvot koetilanteittain:

mean_by_type = tapply(RT, degradation_type, mean)
mean_by_type # näkökentän muutoksen tyyppi

mean_by_window = tapply(RT, window_size, mean)
mean_by_window # näkökentän muutoksen koko

mean_by_both = tapply(RT, list(window_size, degradation_type), mean)
mean_by_both # näkökentän muutoksen tyyppi ja koko

# Histogrammi:
hist(RT, main="Reaktioajan histogrammi", ylab="Frekvenssi", xlab="Reaktioaika (ms)")

# Histogrammiesimerkki ggplot2:lla:
library(ggplot2)
ggplot(data, aes(x=RT)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x = "Reaktioaika (ms)", y = "Osuus")

# Laatikkokuvio:
boxplot(RT~window_size, main="Reaktioaika koetilanteittain", ylab="Reaktioaika (ms)", xlab="Ikkunan koko")

# Laatikkokuvioesimerkki ggplot2:lla:
ggplot(data, aes(x=window_size, y=RT, fill=window_size)) +
  geom_boxplot() +
  labs(y = "Reaktioaika (ms)", x = "Koetilanne") + 
  theme(legend.title=element_blank())

# Numeerinen yhteenveto laatikkokuviosta:
tapply_table = tapply(RT, window_size, mean)
tapply_table # näyttää taulukon konsolissa

# Tallennetaan numeerinen yhteenveto tiedostoon:
write.csv(tapply_table, file="RT_mean_by_window_size.csv") 

# ---

# 3. Mallin rakentaminen

# Luentokalvoissa on käytetty kolmea esimerkkimallia.
# Tallennetaan mallit tässä, tarkastellaan niitä tarkemmin seuraavissa vaiheissa.

# Esimerkki 1: näkökentän muutoksen koon päävaikutus
library(afex)
A1 <- aov_car(RT~Error(subject/window_size), data=data, fun_aggregate=mean)
# Tässä esimerkissä säädetään argumentti fun_aggregate=mean, koska samalle koetilanteelle on aineistossa useampi rivi per osallistuja.

# Esimerkki 2: näkökentän muutoksen koon ja tyypin pää- ja yhteisvaikutukset:
A2 <- aov_car(RT~Error(subject/(degradation_type*window_size)), data=data)

# Esimerkki 3: ryhmän päävaikutus ja näkökentän muutoksen koon ja tyypin pää- ja yhteisvaikutukset:
# Lisätään aineistoon ensin muuttuja kuvaamaan ryhmää.
# Tässä esimerkissä ryhmä on täysin satunnainen, joten sillä ei pitäisi olla vaikutusta reaktioaikaan.
data$group <- rep(1:2, each=84)
data$group <- factor(data$group)
A3 <- aov_car(RT~group + Error(subject/(degradation_type*window_size)), data=data)

# -----

# 5. Oletusten testaaminen

# Vierashavainnot:

# Tarkastellaan laatikkokuviota:
boxplot(RT~window_size*degradation_type, main="Reaktioaika koetilanteittain", ylab="Reaktioaika (ms)", xlab="Ikkunan koko")

# Tarkastellaan laatikkokuvion tuloksia numeerisesti:
library(rstatix)
# Koko aineistolle:
data %>%
  identify_outliers(RT)
# Koetilanteittan:
data %>%
  group_by(window_size, degradation_type) %>%
  identify_outliers("RT")

# Aineistossa ei ole merkittävästi poikkeavia havaintoja.

# Residuaalien normaalijakautuneisuus, esimerkkinä malli 1:
library(performance)
A1_is_norm <- check_normality(A1)
A1_is_norm # näyttää arvion residuaalien normaalijakautuneisuudesta

# Vertailu normaalijakaumaan:
plot(A1_is_norm) # jakauman muoto
plot(A1_is_norm, type = "qq") ## qq-kuvio
plot(A1_is_norm, type = "qq", detrend = TRUE) # qq-kuvio projisoituna

# Residuaalien normaalijakautuneisuus mallille 2:
A2_is_norm <- check_normality(A2)
A2_is_norm

# Residuaalien normaalijakautuneisuus mallille 3:
A3_is_norm <- check_normality(A3)
A3_is_norm

# Residuaalit ovat normaalijakautuneita kaikissa malleissa.

# Varianssien yhtäsuuruus:
check_sphericity(A1) 
check_sphericity(A2)
check_sphericity(A3)

# Varianssit eivät ole yhtäsuuria missään mallissa.
# Käytetään sfäärisyyskorjauksia ANOVAn yhteydessä (ks. seuraava vaihe).

# ---

# 5. Tilastollisen testin tulosten tarkastelu

# Malli 1:
summary(A1)
# Näytön muutoksen koko vaikuttaa reaktioaikaan (F(2,54)=65.45, p[GG]<.001)

# Malli 2:
summary(A2)
# Näytön muutoksen koon ja tyypin yhteisvaikutus reaktioaikaan on tilastollisesti merkitsevä (F2,54)=44.71, p<.001).
# Myös näytön muutoksen tyypin päävaikutus on merkitsevä (F(1,27)=330.93, p<.001).
# Kuten myös näytön muutoksen koon päivaikutus (F(2,54)=65.60, p[GG]<.001).

# Malli 3:
summary(A3)
# Ryhmän vaikutus ei ole tilastollisesti merkitsevä eikä sillä ole myöskään merkitsevää yhteisvaikutusta koetilanteiden kanssa.
# Muut tulokset ovat samat kuin yllä kuvatut.

# ---

# 6. Raportointi

# Interaktiokuvaaja mallille 2:

detach(data)
attach(data)
interaction.plot(window_size, degradation_type, RT,
                 type="b", col=c(1:2),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="Window size",
                 ylab="Reaction time (ms)",
                 main="Interaction plot")

# Numeerinen yhteenveto interaktiokuvaajasta:

tapply_table = tapply(RT, list(degradation_type, window_size), mean)
tapply_table

write.csv(tapply_table, file="RT_mean_by_condition.csv") # tallennetaan tiedostoon


# ---

