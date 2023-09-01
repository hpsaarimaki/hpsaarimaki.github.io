# PSY204 
# HS 1.9.2023
# Mallivastaukset harjoitusmonisteeseen R2.1A

# -----

# 1. Aineiston valmistelu

# Asetetaan työskentelykansio, esim:
setwd("C:/Users/sbhesa/Documents/Opetus/")

# Ladataan ja tallennetaan aineisto:
ageweight <- read.csv("https://bit.ly/PSY204_ageweight")

# Aineiston tarkastelu:
summary(ageweight)

# Sukupuolta (muuttuja MALE) ei ole koodattu faktoriksi. Muutetaan se:
ageweight$MALE <- factor(ageweight$MALE)

# Muuttujat SMOKE1 ja SMOKE2 ovat merkkijonoja, joten muutetaan nekin faktoreiksi:
ageweight$SMOKE1 <- factor(ageweight$SMOKE1)
ageweight$SMOKE2 <- factor(ageweight$SMOKE2)

# Paino (muuttuja WEIGHT) saa kummallisia arvoja (-9??):
which(ageweight$WEIGHT <= 0)
# 31 36 95

# Koodataan nämä havainnot puuttuvina arvoina:
ageweight[which(ageweight$WEIGHT <= 0), "WEIGHT"] <- NA

# Kiinnitetään tietokehys:
attach(ageweight)

# -----

# 2. Kuvioiden tekemisen perusteet

# 2.1 Kuvioiden luominen

# Numeeriselle muuttujalle funktio plot tulostaa sirontakuvion:
plot(WEIGHT)

# Faktorille funktio tulostaa pylväsdiagrammin:
plot(MALE)

# Kaksi numeerista muuttujaa tulostaa sirontakuvion:
plot(AGE, WEIGHT)

# Toinen tapa tuottaa sama kuvio:
# huom! muuttujien järjestys) lue "plot WEIGHT by AGE" eli paino iän funktiona:
plot(WEIGHT~AGE)

# Faktori ja numeerinen muuttuja tuottaa laatikkokuvion kullekin faktorille:
# (huom! muuttujien järjestys)
plot(MALE, WEIGHT)

# Kaksi faktoria tulostaa pylväsdiagrammin:
plot(MALE, SMOKE1)

# Histogrammi (vain numeerisille muuttujille):
hist(WEIGHT)

# Laatikkokuvio (vain numeerisille muuttujille):
boxplot(WEIGHT)

# -

# 2.2 Kuvioiden muokkaaminen

# Lisätään otsikko:
plot(WEIGHT~AGE, main="Paino iän mukaan")

# Muokataan x- ja y-akselien nimiä:
plot(WEIGHT~AGE, main="Paino iän mukaan", xlab="Ikä (v)", ylab="Paino (kg)")

# Muokataan pisteiden tyyliä:
plot(WEIGHT~AGE, main="Paino iän mukaan", xlab="Ikä (v)", ylab="Paino (kg)",
     pch=16)

# Muokataan pisteiden väriä:
plot(WEIGHT~AGE, main="Paino iän mukaan", xlab="Ikä (v)", ylab="Paino (kg)",
     pch=16, col="blue")

# Muokataan akselin pituutta:
plot(WEIGHT~AGE, main="Paino iän mukaan", xlab="Ikä (v)", ylab="Paino (kg)",
     pch=16, col="blue", xlim=c(0,70), ylim=c(0,140))

# Lisätään kuvioon regressiosuora:
plot(WEIGHT~AGE, main="Paino iän mukaan", xlab="Ikä (v)", ylab="Paino (kg)",
     pch=16, col="blue")
abline(lm(WEIGHT~AGE))

# Myös suoraa voidaan muokata:
plot(WEIGHT~AGE, main="Paino iän mukaan", xlab="Ikä (v)", ylab="Paino (kg)",
     pch=16, col="blue")
abline(lm(WEIGHT~AGE), lty="dashed", lwd=2, col="red")

# Tehdään hieno histogrammi:
hist(WEIGHT, main="Paino", xlab="Paino", ylab="")

# Muokataan histogrammin kokonaispinta-alaa:
hist(WEIGHT, main="Paino", xlab="Paino", ylab="", freq=F)

# Automaattisesti valittu palkkien koko:
hist(WEIGHT)

# Voit joko asettaa palkkien lukumäärän:
hist(WEIGHT, breaks=5)

# Tai palkin koon (tässä jokaisen palkin leveys on 20kg):
bins = seq(20,140,by=20)
hist(WEIGHT, breaks=bins)

# Histogrammi normaalijakaumalla:
h <- hist(WEIGHT, freq=F, main="Painon jakauma", xlab="Paino (kg)", ylab="")
x <- min(h$breaks):max(h$breaks)
y <- dnorm(x, mean=mean(WEIGHT, na.rm=T), sd=sd(WEIGHT, na.rm=T))
lines(x, y, col="red", lty="dashed", lwd=3)

# -

# 2.3 Useamman kuvion avaaminen samaan näyttöön

# Lisätään 4 kuviota samaan näyttöön:
layout(matrix(c(1,2,3,4),2,2))
hist(WEIGHT, main="Paino")
hist(HEIGHT, main="Pituus")
plot(SMOKE1, main="Tupakointi mittahetkellä 1")
plot(SMOKE2, main="Tupakointi mittahetkellä 2")

# Ikkunan säädöt voi palauttaa oletusarvoisiksi myös komentorivillä:
graphics.off()

# -

# 2.4 Kuvioiden tallentaminen

# Tallennetaan kuvio pdf-tiedostoon:
pdf(file="hist_weight.pdf")
hist(WEIGHT, main="Paino", xlab="Paino (kg)", ylab="")
dev.off()

# -

# 2.5 Harjoituksia

# Kysymys 1:

hist(WEIGHT, xlim=c(0,150), breaks=6, col="red", xlab="Paino (kg)", bg="grey")

# xlim säätää x-akselin pituuden
# breaks säätää palkkien lukumäärän
# col säätää palkkien värin
# xlab säätää x-akselin nimen
# bg pitäisi säätää taustan värin, mutta ei ainakaan minulla jostain syystä toimi

# -

# Kysymys 2:

hist(WEIGHT, xlim=c(0,150), breaks=6, col="red", xlab="Paino (kg)", bg="grey", main="")

# Antamalla otsikolle (main) tyhjä arvo, saa sen poistettua

# -

# Kysymys 3:

# Arvio: paino nousee iän mukaan

plot(AGE,WEIGHT)

# Arvio vaikuttaa oikealta.

# -

# Kysymys 4:

plot(AGE, WEIGHT, pch=20, col="darkgrey")
abline(lm(WEIGHT~AGE), lty="dashed", lwd=1, col="blue")

# -

# Kysymys 5:

# Käytetään laatikkokuviota:

boxplot(WEIGHT~MALE, names = c("female","male"), xlab="")

# Selvitin laatikkokuvion luokkien nimien vaihtamisen googlella:
# https://statisticsglobe.com/change-axis-labels-of-boxplot-in-r

# -

# Kysymys 6:

# Kysymyksessä 3 on kategorinen muuttuja ja numeerinen muuttuja, jolloin plot tuottaa laatikkokuvion.
# Kysymyksessä 5 on kaksi numeerista muuttujaa, jolloin plot tuottaa sirontakuvion.

# -

# Kysymys 7:

# Googlasin kysymystä "r plot change font color.
# Löysin blogin, jossa oli esimerkkejä, joiden pohjalta kokeilin itse.
# Esimerkiksi sirontakuviolle:

plot(AGE,WEIGHT, col.main="green", col.lab="red", main="Sirontakuvio")

# col.main muuttaa otsikon värin
# col.lab muuttaa akselien nimien värin

# -----

# 3. Kategoristen muuttujien kuvaileva tilastoanalyysi ja kuviot

# 3.1 Ristiintaulukointi

# Frekvenssitaulukko sukupuolelle:
table(MALE)

# Ristiintaulukointi sukupuolelle ja tupakoitsijoille:
table(MALE, SMOKE1)

# Ristiintaulukointi prosenttiluvuilla:
round(prop.table(table(MALE, SMOKE1), margin=1)*100, 1)

# -

# 3.2 Kategoristen muuttujien kuviot

# Laatikkokuvio kategorioiden frekvensseistä (Kuvio 29):
plot(MALE)

# Muokattu laatikkokuvio (Kuvio 30):
plot(MALE, col=c("red", "blue"), xlab="Gender")

# Laatikkokuviot kahdelle kategoriselle muuttujalle (Kuviot 31 ja 32):
plot(MALE, SMOKE1)
plot(MALE, SMOKE1, col=c("red","blue"), xlab="Gender", ylab="Smoking")

# -

# 3.3 Tehtäviä

# Kysymys 8:

table(SMOKE1, SMOKE2)

# Mittaushetkien 1 ja 2 välillä 
# Mittaushetkellä 1 oli 48 tupakoitsijaa, joista vain 22 tupakoi mittaushetkellä 22.
# Eli tupakoinnin lopetti 26 henkilöä.
# Sen sijaan tupakoinnin mittaushetkien välillä oli aloittanut 35 henkilöä.

# -

# Kysymys 9:

# Esimerkiksi:

plot(SMOKE1,SMOKE2, xlab="Smoking at time point 1", ylab="Smoking at time point 2", main="Frequency of smoking")

# -----

# 4. Jatkuvien muuttujien kuvaileva tilastoanalyysi ja kuviot

# 4.1 Kuvaileva tilastoanalyysi

# Kokeillaan funktioita summary ja describe:
summary(ageweight)
library(psych)
describe(ageweight)

# Kuvailevia tuloksia yksittäisille muuttujille:
mean(WEIGHT, na.rm=T) # keskiarvo
var(WEIGHT, na.rm=T) # varianssi

# Painon keskiarvot miehille ja naisille erikseen:
tapply(WEIGHT, MALE, mean, na.rm=T)

# Painon keskiarvo miehille ja naisille tupakoitsijoiden ja ei-tupakoivien ryhmissä:
tapply(WEIGHT, list(MALE, SMOKE1), mean, na.rm=T)

# Tallennetaan taulukko tiedostoon:
taulukko <- tapply(WEIGHT, list(MALE, SMOKE1), mean, na.rm=T)
write.csv(taulukko, "taulukko.csv")

# Tallennetaan keski- ja hajontaluvut koko aineistolle:
keskiluvut <- describe(ageweight)
write.csv(keskiluvut, "ageweight_keskiluvut.csv")

# -

# 4.2 Kuviot

# Laatikkokuvio:
boxplot(WEIGHT)

# Paino eri sukupuolikategorioissa:
boxplot(WEIGHT~MALE)

# Painon histogrammeja:
hist(WEIGHT)
hist(WEIGHT, breaks=20)

# -

# 4.3 Harjoituksia

# Kysymys 10:

mean(HEIGHT, na.rm=T)
sd(HEIGHT, na.rm=T)

# Pituuden keskiarvo 1.73 ja keskihajonta 0.10

# -

# Kysymys 11:

# Tässä yksi vaihtoehto kysymyksen ratkaisuksi, jos haluaa keskiarvon ja -hajonnan samaan taulukkoon.
# Kootaan pituuden keskiarvo ja hajonta sukupuolikategorioittain tietokehykseen:

pituustaulukko <- data.frame(rbind(tapply(HEIGHT, MALE, mean, na.rm=T), tapply(HEIGHT, MALE, sd, na.rm=T)))
pituustaulukko # tämä tulostaa meille taulukon sisällön

# En ollut tyytyväinen sarakkeiden ja rivien nimiin, joten muutetaan ne kuvaavammiksi:

colnames(pituustaulukko) <- c("female", "male")
rownames(pituustaulukko) <- c("keskiarvo", "keskihajonta")

# Tarkastellaan lopullista taulukkoa:

pituustaulukko

# Nyt näyttää hyvältä, tallennetaan taulukko:

write.csv(pituustaulukko, "Pituus_sukupuolen_mukaan.csv")

# -

# Kysymys 12:

hist(HEIGHT)

# -

# Kysymys 13:

h <- hist(HEIGHT, freq=F, ylim=c(0,4.5))
x <- seq(min(h$breaks),max(h$breaks), by=0.025)
y <- dnorm(x, mean=mean(HEIGHT, na.rm=T), sd=sd(HEIGHT, na.rm=T))
lines(x,y,col="red",lty="dashed",lwd=3)

# Tässä tehtävässä x:n määrittelyssä piti soveltaa, koska osion 2.2 esimerkin mukaan
# tehty lukujono antaa vain yhden arvon. Tämä johtuu siitä, että : antaa kaikki kokonaisluvut
# pyydetyltä väliltä, mutta pituuden arvot ovat metreinä, ja siksi minimin ja maksimin välille
# ei osu kuin yksi kokonaisluku.
# Tästä syystä :-merkin sijaan käytettiin funktiota seq, jolla saatiin säädetty haluttu lukujen väli.

# -

# Kysymys 14:

# Esimerkiksi:

hist(HEIGHT, freq=F,
     breaks=20,                   # muuta palkkien kokoa
     ylim=c(0,5),                 # muuta y-akselin kokoa
     main="Pituuden jakauma",     # muuta otsikkoa
     xlab="Height (m)", ylab="",   # muuta akseleiden nimiä
     col="pink")                  # muuta väriä
x <- seq(min(h$breaks),max(h$breaks), by=0.025)
y <- dnorm(x, mean=mean(HEIGHT, na.rm=T), sd=sd(HEIGHT, na.rm=T))
lines(x,y,col="red",lty="dashed",lwd=3)

# -

# Kysymys 15:

png(file="Pituuden_jakauma.png")
hist(HEIGHT, freq=F,
     breaks=20,                   # muuta palkkien kokoa
     ylim=c(0,5),                 # muuta y-akselin kokoa
     main="Pituuden jakauma",     # muuta otsikkoa
     xlab="Height (m)", ylab="",   # muuta akseleiden nimiä
     col="pink")                  # muuta väriä
x <- seq(min(h$breaks),max(h$breaks), by=0.025)
y <- dnorm(x, mean=mean(HEIGHT, na.rm=T), sd=sd(HEIGHT, na.rm=T))
lines(x,y,col="red",lty="dashed",lwd=3)
dev.off()

# -

# Kysymys 16:

boxplot(HEIGHT)

# A) mediaani on hieman yli 1.7m
# B) jakauma näyttää melko normaalilta, hajontaa on enemmän korkeammilla arvoilla

# -

# Kysymys 17:

boxplot(HEIGHT~MALE, main="Height by Gender", ylab="Height (m)", xlab="Gender")

# miehet ovat keskimäärin naisia pidempiä

# -

# Kysymys 18:

layout(matrix(c(1,2)))
boxplot(WEIGHT, main="Box plot of Weight", ylab="Weight (kg)", col="grey")
hist(WEIGHT, main="Histogram of Weight", xlab="Weight (kg)", ylab="", col="grey")

# histogrammi on parempi tapa tarkastella jakaumaa

# -

# Kysymys 19:

graphics.off()
pdf(file="Painon_jakauma.pdf")
hist(WEIGHT, main="Painon jakauma", 
     xlab="Paino (kg)", ylab="Frekvenssi", 
     breaks=12, col="grey")
dev.off()

# -----

# 5. Soveltavia harjoituksia

# Ladataan aineisto:
naming <- read.csv("https://bit.ly/PSY204_naming", sep="")

# Tarkastellaan aineistoa:
summary(naming)

# Muutetaan faktorit faktoreiksi:
naming$word.type <- factor(naming$word.type)
naming$male <- factor(naming$male)

# Muutetaan iq:n puuttuvat arvot:
naming$iq[which(naming$iq == 999)] <- NA

# Tarkistetaan muutokset:
summary(naming)

# Poistetaan aiemman tietokehyksen kiinnitys
detach(ageweight)

# Kiinnitetään uusi tietokehys
attach(naming)

# -

# Kysymys 20:

plot(word.type, ms)

# A) Harvinaisten sanojen lukuajat ovat hitaampia kuin tavallisten sanojen.

layout(matrix(c(1,2)))
plot(word.type, ms, data=naming[which(male=="0"),], main="Female")
plot(word.type, ms, data=naming[which(male=="1"),], main="Male")

# B) Sukupuoli ei näytä vaikuttavan eroihin tavallisten ja harvinaisten sanojen lukunopeudessa.

# -

# Kysymys 21:

# Poimitaan osa aineistosta:

naming_osa <- naming[1:1000, c("iq", "hrs", "male")]

# Poistetaan vanha kiinnitetty aineisto ja kiinnitetään uusi:

detach(naming)
attach(naming_osa)

# -

# Kysymys 22:

naming_osa$reading.group <- 'low'
naming_osa$reading.group[which(hrs > 3.5)] <- 'medium'
naming_osa$reading.group[which(hrs > 4.5)] <- 'high'
naming_osa$reading.group <- factor(naming_osa$reading.group)

# Tarkastellaan aineistoa:

head(naming_osa)
summary(naming_osa)

# Kiinnitetään aineisto uudestaan, koska lisättiin uusi muuttuja:

attach(naming_osa)

# -

# Kysymys 23:

table(male, reading.group)
plot(table(male, reading.group))

# Ei eroja miesten ja naisten välillä erilaisiin lukuryhmiin jakautumisessa.

# - 

# Kysymys 24:

boxplot(hrs~male)

# Samat tulkinta kuin kysymyksessä 23: ei eroja.

# -

# Kysymys 25:

boxplot(iq~male)

# Ei sukupulieroja älykkyydessä.

# -

# Kysymys 26:

h <- hist(iq, freq=F, breaks=20)
x <- seq(min(h$breaks),max(h$breaks), by=0.01)
y <- dnorm(x, mean=mean(iq, na.rm=T), sd=sd(iq, na.rm=T))
lines(x,y,col="red",lty="dashed",lwd=3)

# Älykkyyspistemäärä näyttää noudattavan normaalijakaumaa hyvin.

# -

# Kysymys 27:

# Kuvailevat tulokset lukunopeudelle:

mean <- tapply(naming$ms, naming$word.type, mean, na.rm=T)
sd <- tapply(naming$ms, naming$word.type, sd, na.rm=T)
reading_times <- rbind(mean, sd)
reading_times
write.csv(reading_times, "lukunopeus.csv")

# Kuvailevat tulokset taustamuuttujille:

bg_desc <- describe(naming_osa)
bg_desc
write.csv(bg_desc, "taustamuuttujat_tunnusluvut.csv")

# Frekvenssit taustamuuttujille:

bg_freq <- summary(naming_osa)
bg_freq
write.csv(bg_freq, "taustamuuttujat_frekvenssit.csv")
