# R-harjoitusmoniste R4.1
# Toistettujen mittausten varianssianalyysi
# PSY204 syksy 2024
# HS 12.9.2024

# ---

# Ennen aloitusta siirry haluamaasi työskentelykansioon, esim:
setwd("C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Harjoitukset")

# ---

# 1. Aineiston lataaminen ja valmistelu

# Lataa aineisto:
data <- read.csv('https://bit.ly/PSY204_hitfeelshit1')

# Tarkastele aineistoa:
summary(data)

# Poista turha sarake X:
data$X <- NULL

# Kysymys 1:
summary(data)
# Kategoriset muuttujat eli faktorit: 
# RYHMA (2 luokkaa), HARJOITUS (6 luokkaa), VETO (4 luokkaa)
# Numeeriset muuttujat:
# UUPUMUS, EPAMIELLYTTAVYYS, KIIHTYNEISYYS

# Kysymys 2:
# Jokainen osallistuja kuuluu joko ryhmään HIIT tai MICT, joten RYHMA on lohkotekijä.
# Käsittelytekijät ovat harjoitus ja veto. Jokaisessa kuudessa harjoituksessa on neljä vetoa.

# Kysymys 3:

# Muutetaan kategoriset muuttujat ID, RYHMA, HARJOITUS ja VETO faktoreiksi.
data$ID <- factor(data$ID)
data$RYHMA <- factor(data$RYHMA)
data$VETO <- factor(data$VETO)
data$HARJOITUS <- factor(data$HARJOITUS)
summary(data)
# Puuttuvia eikä muuten epäilyttäviä arvoja ei näytä olevan:
boxplot(data$UUPUMUS)
boxplot(data$EPAMIELLYTTAVYYS)
boxplot(data$KIIHTYNEISYYS)

# Kiinnitetään aineisto:
attach(data)

# ---

# 2. Kuvailevat tulokset

# Kysymys 4:
length(unique(data$ID))
# Faktori ID saa 26 eri arvoa, joten aineistossa on 26 osallistujaa.

# Kysymys 5:
hist(EPAMIELLYTTAVYYS)
# Epämiellyttävyys on kohtuullisen normaalisti jakautunut.

# Kysymys 6:
boxplot(EPAMIELLYTTAVYYS ~ RYHMA)

# Kysymys 7:
epamiellyttavyys_ryhmittain = tapply(EPAMIELLYTTAVYYS, RYHMA, mean)
epamiellyttavyys_ryhmittain

# Kysymys 8:
boxplot(EPAMIELLYTTAVYYS ~ HARJOITUS)
# Koettu epämiellyttävyys on keskimäärin korkea kahden ensimmäisen harjoituskerran aikana, mutta tasaantuu sitten.

# Kysymys 9:
boxplot(EPAMIELLYTTAVYYS ~ VETO)
# Koettu epämiellyttävyys kasvaa harjoituksen aikana.

# -----

# 3. Toistettujen mittausten varianssianalyysi

# -

# 3.1 Muuttuuko koettu epämiellyttävyys harjoituskerran aikana?

# -

# Mallin rakentaminen:

# Ladataan kirjasto (asenna 'afex' tarvittaessa):
library(afex)

# Malli A1:
A1 <- aov_car(EPAMIELLYTTAVYYS~Error(ID/VETO), data=data, fun_aggregate=mean)

# -

# Oletusten testaaminen:

# Vierashavainnot;
library(dplyr)
library(rstatix)  # lataa tarvittava kirjasto, asenna tarvittaessa ensin

# Koko aineisto
data %>%
  identify_outliers(EPAMIELLYTTAVYYS)
# ei vierashavaintoja

# Koetilanteittan harjoitusryhmän mukaan:
vierashavainnot <- data %>%
  group_by(RYHMA, HARJOITUS, VETO) %>%
  identify_outliers("EPAMIELLYTTAVYYS")
print(vierashavainnot, n=23)

# Vierashavainnot eivät ole merkitsevästi poikkeavia.

# Jäännöstermien normaalijakautuneisuus:
library(performance)     # ladataan avuksi
A1_is_norm <- check_normality(A1)
A1_is_norm
plot(A1_is_norm, type="qq")
# Jäännöstermit ovat normaalisti jakautuneet

# Varianssien yhtäsuuruus eli sfäärisyys:
check_sphericity(A1)
# Sfäärisyys ei toteudu, tämä huomioidaan analyysissa.

# -

# Tulosten tarkastelu:

# Tutkitaan tunnuslukuja:
summary(A1)
# Efektikoko:
library(effectsize)
eta_squared(A1)

# Koettu epämiellyttävyys muuttuu tilastollisesti merkitsevästi yhden harjoituskerran aikana (F(3,75)=59.39, p[GG]<.001).
# Tulosten tarkasteluun käytettiin Greenhouse-Geisserin sfäärisyyskorjausta, koska varianssit eivät olleet joka solussa yhtä suuret.

# Tarkastellaan eri vetojen epämiellyttävyyden keskiarvoja:
tapply(EPAMIELLYTTAVYYS, VETO, mean)
# Epämiellyttävyys kasvaa harjoituksen aikana (veto 1: 3.6, veto 2: 4.4, veto 3: 5.1, veto 4: 5.5).

# Näytetään tulokset tuunattuna laatikkokuviona:
library(ggplot2) # ladataan kirjasto, asenna tarvittaessa
ggplot(data, aes(x=VETO, y=EPAMIELLYTTAVYYS)) +
  geom_boxplot() +
  labs(y = "Epämiellyttävyys", x = "Vedon järjestysnumero") + 
  theme(legend.title=element_blank())

# Posthoc-testit:
library(emmeans)
A1_posthoc <- emmeans(A1, specs=pairwise ~ VETO, adjust = "holm")
A1_posthoc
# Kaikkien vetoparien erot ovat tilastollisesti merkitseviä.

# ---

# 3.2 Vaihteleeko harjoituskerran aikana koettu epämiellyttävyys koko harjoitusjakson aikana aikana?

# -

# Mallin rakentaminen:

# Malli A2:
A2 <- aov_car(EPAMIELLYTTAVYYS~Error(ID/HARJOITUS*VETO), data=data)

# -

# Oletusten testaaminen:

# Vierashavainnot tarkasteltiin jo kohdassa 3.1, ne eivät ole merkitsevästi poikkeavia.

# Jäännöstermien normaalijakautuneisuus:
A2_is_norm <- check_normality(A2)
A2_is_norm
plot(A2_is_norm, type="qq")
# Jäännöstermit eivät ole normaalisti jakautuneet

# Varianssien yhtäsuuruus eli sfäärisyys:
check_sphericity(A2)
# Sfäärisyys ei toteudu, tämä huomioidaan analyysissa.

# -

# Tutkitaan tunnuslukuja:
summary(A2)
# Efektikoko:
eta_squared(A2)


# Harjoituksen aikaisen epämiellyttävyyden kokeminen ei muutu harjoittelun myötä (F(15,375)=1.36, p[GG]=.23).
# Harjoituksen aikana epämiellyttävyys muuttuu (F(3,75)=59.4, p[GG]<.001).
# Epämiellyttävyys muuttuu myös harjoituskertojen myötä (F(5,125)=6.4, p[GG]<.01).

# Tarkastellaan eri vetojen epämiellyttävyyden keskiarvoja eri harjoituskerroilla:
tapply(EPAMIELLYTTAVYYS, list(VETO,HARJOITUS), mean)

# Näytetään tulokset interaktiokuviona:
interaction.plot(VETO, HARJOITUS, EPAMIELLYTTAVYYS,
                 type="b", col=c(1:6),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="VETO",
                 ylab="EPAMIELLYTTAVYYS",
                 main="Interaktiokuvio")
# Kuvio näyttää eri harjoituskertojen aikaisen koetun epämiellyttävyyden muutoksen.
# Epämiellyttävyys kasvaa aina harjoituskerran aikana.
# Ensimmäisessä harjoituksessa koettu epämiellyttävyys on suurin.
# Koettu epämiellyttävyys laskee harjoittelun myötä.

# Post hoc-testit:
# Post hoc -testi vetoparien vertailulle tehtiin jo osiossa 3.1.
# Testataan vielä harjoituskertojen parittaiset vertailut:
A2_posthoc <- emmeans(A2, specs=pairwise ~ HARJOITUS, adjust = "holm")
A2_posthoc$contrasts
# Bonferroni-Holm-korjauksen jälkeen merkitsevä ero löytyy vain 
# ensimmäisen ja viimeisen harjoituksen väliltä (t(25)=3.41, p<.05).


# ---

# 3.3 Onko ryhmien välillä eroa siinä, miten epämiellyttävyys
# vaihtelee harjoituskerran aikana?

# -

# Mallin rakentaminen:
A3 <- aov_car(EPAMIELLYTTAVYYS~RYHMA + Error(ID/VETO), data=data, fun_aggregate = mean)

# -

# -

# Oletusten testaaminen:

# Vierashavainnot tarkasteltiin jo kohdassa 3.1, ne eivät ole merkitsevästi poikkeavia.

# Jäännöstermien normaalijakautuneisuus:
A3_is_norm <- check_normality(A3)
A3_is_norm
plot(A3_is_norm, type="qq")
# Jäännöstermit eivät ole normaalisti jakautuneet

# Varianssien yhtäsuuruus eli sfäärisyys:
check_sphericity(A3)
# Sfäärisyys ei toteudu, tämä huomioidaan analyysissa.

# -

# Tulosten tarkastelu ja raportointi:

# Malli A3:
summary(A3)
# Efektikoko:
eta_squared(A3)

# Ryhmät eroavat toisistaan vain harjoituskerran sisäisen muutoksen osalta (ryhmän ja vedon yhteisvaikutus, F(3, 72)=8.6, p[GG]<.01).
# Epämiellyttävyyden muutos harjoituskerran sisällä riippuu siis ryhmästä.
# Vedon päävaikutus on merkitsevä, eli harjoituskerran sisällä tapahtuu myös keskimäärin muutosta (F(3,72)=77.4, p[GG]<.001)

# Näytetään tulokset interaktiokuviona:
interaction.plot(VETO, RYHMA, EPAMIELLYTTAVYYS,
                 type="b", col=c(1:6),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="VETO",
                 ylab="EPAMIELLYTTAVYYS",
                 main="Interaktiokuvio")
# Harjoituskerran aikana koettu epämiellyttävyys kasvaa jyrkemmin HIIT-ryhmässä.

# Post hoc -testi:
# Vetoparien vertailu tehtiin jo kohdassa 3.1.
# Testataan vielä yhdysvaikutuksen tulkitsemisen avuksi:
A3_posthoc <- emmeans(A3, specs=pairwise ~ RYHMA:VETO, adjust="holm")
A3_posthoc$contrasts
