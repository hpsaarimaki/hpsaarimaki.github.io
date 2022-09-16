# R-harjoitusmoniste R2.2
# Toistettujen mittausten varianssianalyysi I
# PSY204 syksy 2022
# HS 3.9.2022

# ---

# Ennen aloitusta siirry haluamaasi työskentelykansioon, esim:
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ---

# 1. Aineiston lataaminen ja valmistelu

# Ennen aloitusta asenna kirjasto 'nummenmaa' koneellesi:
install.packages("remotes")
library(remotes)
install_url("http://emotion.utu.fi/wp-content/uploads/2019/11/nummenmaa_1.0.tar.gz",dependencies=TRUE)

# Ota kirjasto session käyttöön:
library(nummenmaa)

# Lataa aineisto:
data <- hitfeelshit1

# Tarkastele aineistoa:
summary(data)

# Kysymys 1:
# Kategoriset muuttujat eli faktorit: 
# RYHMA (2 luokkaa), HARJOITUS (6 luokkaa), VETO (4 luokkaa)
# Numeeriset muuttujat:
# UUPUMUS, EPAMIELLYTTAVYYS, KIIHTYNEISYYS

# Kysymys 2:
# Jokainen osallistuja kuuluu joko ryhmään HIIT tai MICT, joten RYHMA on lohkotekijä.
# Käsittelytekijät ovat harjoitus ja veto. Jokaisessa kuudessa harjoituksessa on neljä vetoa.

# Kysymys 3:
# Muuttuja ID on koodattu numeerisena, muutetaan se faktoriksi.
data$ID <- factor(data$ID)
# Puuttuvia eikä muuten epäilyttäviä arvoja ei näytä olevan.

# Kiinnitetään aineisto:
attach(data)

# ---

# 2. Kuvailevat tulokset

# Kysymys 4:
length(unique(data$ID))
# Faktori ID saa 26 eri arvoa, joten aineistossa on 26 osallistujaa.

# Kysymys 5:
# Tarkastellaan aineiston keskilukuja:
library(psych) # ladataan kirjasto - asenna tarvittaessa
keskiluvut <- describe(data)
keskiluvut # näytä mitä tallennettiin
write.csv(keskiluvut, file="hitfeelshit1_keskiluvut.csv") # tallennetaan tiedostoon

# Kysymys 6:
hist(EPAMIELLYTTAVYYS)
# Epämiellyttävyys ei vaikuta täysin normaalisti jakautuneelta. Tarkastellaan tätä lisää myöhemmin.

# Kysymys 7:
boxplot(EPAMIELLYTTAVYYS ~ RYHMA)

# Kysymys 8:
epamiellyttavyys_ryhmittain = tapply(EPAMIELLYTTAVYYS, RYHMA, mean)
epamiellyttavyys_ryhmittain

# Kysymys 9:
boxplot(EPAMIELLYTTAVYYS ~ HARJOITUS)
# Koettu epämiellyttävyys on keskimäärin korkea kahden ensimmäisen harjoituskerran aikana, mutta tasaantuu sitten.

# Kysymys 10:
boxplot(EPAMIELLYTTAVYYS ~ VETO)
# Koettu epämiellyttävyys kasvaa harjoituksen aikana.

# -----

# 3. Mallin rakentaminen

# Ladataan kirjasto (asenna 'afex' tarvittaessa):
library(afex)

# Malli A1:
A1 <- aov_car(EPAMIELLYTTAVYYS~Error(ID/VETO), data=data, fun_aggregate=mean)

# Malli A2:
A2 <- aov_car(EPAMIELLYTTAVYYS~Error(ID/HARJOITUS*VETO), data=data)

# Malli A3:
A3 <- aov_car(EPAMIELLYTTAVYYS~RYHMA + Error(ID/VETO), data=data, fun_aggregate = mean)


# 4. Oletusten testaaminen

# Kysymys 11: Vierashavainnot

# Ladataan tarvittava kirjasto, asenna tarvittaessa:
library(rstatix)

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

# Kysymys 12: normaalijakautuneisuus

# Ladataan tarvittava kirjasto, asenna tarvittaessa:
library(performance)

# Malli A1:
A1_is_norm <- check_normality(A1)
A1_is_norm
# residuaalit normaalisti jakautuneet

# Malli A2:
A2_is_norm <- check_normality(A2)
A2_is_norm
plot(A2_is_norm, type="qq")
# residuaalit ei normaalisti jakautuneet

# Malli A2:
A3_is_norm <- check_normality(A3)
A3_is_norm
plot(A3_is_norm, type="qq")
# residuaalit ei normaalisti jakautuneet

# Kysymys 13: varianssien yhtäsuuruus
check_sphericity(A1)
check_sphericity(A2)
check_sphericity(A3)
# varianssit eivät ole yhtä suuria missään mallissa

# Kysymys 14:
# Varianssien yhtäsuuruusoletuksen saa korjattua sfäärisyyskorjauksilla
# Mutta ryhmäkoot ovat varsin pieniä, vain 13 osallistujaa per ryhmä
# Lisäksi normaalijakautuneisuusoletus on ongelma
# Toistettujen mittausten varianssianalyysin sijaan kannattaisi harkita monitasoregressiomalleja

# ---

# 5. Tilastollisen testin tulosten tarkastelu

# Malli A1:
# Tutkitaan tunnuslukuja:
summary(A1)
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

# Malli A2:
# Tutkitaan tunnuslukuja:
summary(A2)
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

# Malli A3:
summary(A3)
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
