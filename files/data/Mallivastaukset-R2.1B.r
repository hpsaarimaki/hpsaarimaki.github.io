# PSY204
# R-harjoitusmoniste R2.1B
# Mallivastaukset
# Heini Saarimäki 1.9.2023

# -----

# 1. Aineiston valmistelu

# ---

# 1.2 Aineiston lataaminen ja valmistelu

setwd('C:/Users/sbhesa/Documents/Opetus/')

deprivation <- read.csv('https://bit.ly/PSY204_deprivation')

# Aineiston tarkastelu

head(deprivation)		# 10 muuttujaa
dim(deprivation)		# 47 riviä (eli 47 osallistujaa, yksi per rivi)

summary(deprivation)	

# kategorisia muuttujia ID, age, education ja ht_group ei vielä ole koodattu faktoreina
# --> muutetaan ID, age, education ja ht_group faktoreiksi

deprivation$ID <- factor(deprivation$ID)
deprivation$age <- factor(deprivation$age)
deprivation$education <- factor(deprivation$education)
deprivation$ht_group <- factor(deprivation$ht_group)

summary(deprivation)	# kaikki näyttää olevan nyt kunnossa

# Tarkastetaan puuttuvien arvojen koodaus:

summary(deprivation)
boxplot(deprivation[5:7])	# otetaan laatikkokuviot merkkitestille kaikilla mittaushetkillä
boxplot(deprivation[8:10])	# vastaavat laatikkokuviot Bentonin testille

# äärimmäisiä vierashavaintoja ei ole (ainakaan väärinkoodattujen puuttuvien arvojen muodossa)
# eli puuttuvien arvojen koodausta ei tarvitse korjata

# kun olet valmistellut aineiston, kiinnitä se:

attach(deprivation)

# ---

# 2. Aineiston kuvailu

# tarkistetaan ryhmien frekvenssit kaikissa kategorisissa muuttujissa:

summary(deprivation[2:4])

# tarkastellaan vielä frekvenssejä ristiintaulukoinnin avulla:

table(age,education)
table(age, ht_group)
table(education, ht_group)

# kuvailevien tulosten taulukko jatkuville muuttujille:

library('psych')			# ladataan kirjasto 'psych', josta saadaan funktio 'describe' käyttöön
describe(deprivation[5:10])	# otetaan kuvailevat tulokset jatkuville muuttujille, poiminta sarakkeiden numeroiden avulla

# tallennetaan kuvailevat tulokset mahdollista jatkokäyttöä varten:

deprivation.descriptives <- describe(deprivation[5:10])
write.csv(deprivation.descriptives, "deprivation_tunnusluvut.csv")

# jatkuvien muuttujien visualisointia:

layout(matrix(c(1,2,3,4,5,6),3,2))		# luodaan ensin kuusilokeroinen näyttö
boxplot(digitsymbol_1, ylim=c(0,80), main="Merkkitesti, mittaushetki 1")
boxplot(digitsymbol_2, ylim=c(0,80), main="Merkkitesti, mittaushetki 2")
boxplot(digitsymbol_3, ylim=c(0,80), main="Merkkitesti, mittaushetki 3")
boxplot(bentonerror_1, ylim=c(0,14), main="Bentonin testi, mittaushetki 1")
boxplot(bentonerror_2, ylim=c(0,14), main="Bentonin testi, mittaushetki 2")
boxplot(bentonerror_3, ylim=c(0,14), main="Bentonin testi, mittaushetki 3")

# jatkuvien muuttujien histogrammit:

layout(matrix(c(1,2,3,4,5,6),3,2))		# luodaan ensin kuusilokeroinen näyttö
hist(digitsymbol_1, main="Merkkitesti, mittaushetki 1")
hist(digitsymbol_2, main="Merkkitesti, mittaushetki 2")
hist(digitsymbol_3, main="Merkkitesti, mittaushetki 3")
hist(bentonerror_1, main="Bentonin testi, mittaushetki 1")
hist(bentonerror_2, main="Bentonin testi, mittaushetki 2")
hist(bentonerror_3, main="Bentonin testi, mittaushetki 3")


# -----


# 3. T-testit

# --

# 3.1 Yhden otoksen t-testi

# verrataan otoskeskiarvoa tunnettuun populaation keskiarvoon: 

summary(digitsymbol_1)		# otoskeskiarvo 42.23, populaation keskiarvo 40

# onko ero tilastollisesti merkitsevä?
# käytetään yhden otoksen t-testiä:

t.test(digitsymbol_1, mu=40)

# --> otoskeskiarvo ei poikkea tilastollisesti merkitsevästi populaatiokeskiarvosta (t(46)=1.33, p=.19)

# ---

# 3.2 Riippumattomien otosten t-testi

# t-testi ikäryhmien vertailemiseksi merkkitestissä mittaushetkellä 1:

t.test(digitsymbol_1 ~ age)	# kategorinen muuttuja tulee merkin ~ jälkeen 

# --> ryhmien keskiarvot eivät eroa toisistaan (t(35)=0.27, p>.79)

# näytetään tulokset laatikkokuvion avulla:

boxplot(digitsymbol_1 ~ age, xlab="Ikäryhmä", ylab="Merkkitestin pistemäärä")

# muokataan kuvaa julkaisukelpoisemmaksi:

# Tehdään laatikkokuviosta hieman julkaisukelpoisempi:

library(ggplot2)
ggplot(deprivation, aes(x=age, y=digitsymbol_1, fill=age)) +
  geom_boxplot() +
  labs(y = "Merkkitestin pistemäärä", x = "Ikäryhmä") + 
  theme(legend.title=element_blank())

# ---

# 3.3 Toistettujen mittausten t-testi

# Toistettujen mittausten t-testi univajeen vaikutuksen testaamiseksi:

t.test(digitsymbol_1, digitsymbol_2, paired=T)

# Mittaushetkien 1 ja 2 välillä ei ole eroa (t(46)=-0.79, p=0.44).

# Laatikkokuvio:

boxplot(digitsymbol_1, digitsymbol_2, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")

# ---

# 3.4 Harjoituksia

# Kysymys 1:

t.test(digitsymbol_1 ~ ht_group)

# Ryhmien välillä ei ole eroa (t(44)=-0.17, p=0.86).

# -

# Kysymys 2:

# Mittaushetket 1 ja 3:

t.test(digitsymbol_1, digitsymbol_3, paired=TRUE)

# Mittaushetkien 1 ja 3 välillä on tilastollisesti merkitsevä ero (t(46)=-6.9, p<.001).
# Tarkastellaan eroja laatikkokuviolla:

boxplot(digitsymbol_1, digitsymbol_3, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")

# Kuviosta huomataan, että pistemäärät ovat korkeammat mittaushetkellä 3.
# Kokeen aikana tapahtuu todennäköisesti oppimista.

# Mittaushetket 2 ja 3:

t.test(digitsymbol_2, digitsymbol_3, paired=TRUE)

# Mittaushetkien 1 ja 3 välillä on tilastollisesti merkitsevä ero (t(46)=-10.6, p<.001).
# Tarkastellaan eroja laatikkokuviolla:

boxplot(digitsymbol_2, digitsymbol_3, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")

# Kuviosta huomataan, että pistemäärät ovat korkeammat mittaushetkellä 3.
# Eli sama todennäköisesti oppimisvaikutuksesta johtuva eroaa näkyy mittaushetkien 2 ja 3 välillä.

# Yhteenvetona voidaan todeta, että merkkitestissä tapahtuu oppimista kokeen aikana.
# On mahdollista, että emme saaneet tilastollisesti merkitsevää univajeen vaikutusta siksi, 
# että kokeen aikana tapahtuva oppiminen peitti univajeen vaikutuksen alleen.


# -----

# 4. Riippumattomien otosten varianssianalyysi

# --

# 4.1 4.1 Yksisuuntainen ANOVA

# Yksisuuntainen ANOVA koulutusryhmien eroille merkkitestissä:

A1 <- aov(digitsymbol_1 ~ education, data=deprivation)

# Tulostetaan ANOVA:n tulokset

summary(A1)

# --> ryhmien välillä ei ole merkitsevää eroa (F(2,44)=2.16, p=.13)

# Tulostetaan ryhmien keskiarvot

model.tables(A1, "means")

# Tulosten visualisointi:

boxplot(digitsymbol_1 ~ education, xlab="Koulutusryhmä", ylab="Merkkitestin pistemäärä",
        main="Merkkitestin pistemäärä eri koulutusryhmissä", col="grey")

# -

# 4.1.1 Oletusten testaaminen

# Ryhmien otoskoot:
summary(education)

# Diagnostiset testit:
layout(matrix(1:4,2,2))
plot(A1)

# Normaalijakautuneisuuden testit:

# Shapiro-Wilkin testi:
shapiro.test(digitsymbol_1)

# Kolmogorov-Smirnovin testi:
ks.test(digitsymbol_1, "pnorm", mean=mean(digitsymbol_1), sd=sd(digitsymbol_1))

# Varianssien yhtäsuuruuden testit (kirjastosta 'car'):
library(car)

# Levenen testi:
leveneTest(digitsymbol_1 ~ education)

# Bartlettin testi:
bartlett.test(digitsymbol_1 ~ education)

# --

# 4.1.2 Tulosten raportointi

# Laatikkokuviot:
graphics.off # nollataan diagnostisten kuvioiden kohdalla asetettu kuvioiden asettelu
boxplot(digitsymbol_1~education)

# Plotmeans-kuvio (keskiarvot ja luottamusvälit)
library(gplots)
plotmeans(digitsymbol_1 ~ education, ylim=c(10,70))

# ---

# # 4.2 Kaksisuuntainen ANOVA

# Luodaan malli:
A2 <- aov(digitsymbol_1 ~ age * ht_group, data=deprivation)

# Tarkastellaan tuloksia:
summary(A2)

# --> vain iän ja hormonihoidon yhteisvaikutus on merkitsevä (F(1,43)=832.2, p<.05).

# Tarkastellaan ryhmien keskiarvoja:
model.tables(A2, "means")

# Tarkastellaan diagnostisia kuvioita:
layout(matrix(1:4,2,2))
plot(A2)

# Laatikkokuvio:
graphics.off()
boxplot(digitsymbol_1 ~ age * ht_group)

# Perusversio interaktiokuviosta:
interaction.plot(age, ht_group, digitsymbol_1)

# Julkaisukelpoiseksi tuunattu versio:
interaction.plot(age, ht_group, digitsymbol_1, type="b", col=c(1:3), leg.bty="o", 
                 leg.bg="beige", lwd=2, pch=c(18, 24, 22), xlab="Ikäryhmä", ylab="Merkkitestin pistemäärä")

# --

# 4.2.1 Neliösummien tyyppi

# Otetaan tyypin III neliösummat mallille A2:
library(car)
Anova(A2, type=3)

# --

# 4.2.2 Post hoc -vertailut

# Käytetään esimerkin vuoksi koulutustasoa ja merkkitestiä, vaikka ANOVA ei antanutkaan
# merkitseviä eroja koulutustason suhteen tässä testissä.

# Joko ilman monivertailukorjausta (ei suositeltavaa!):
pairwise.t.test(digitsymbol_1, education, p.adj="none")

# Tai erilaisilla monivertailukorjauksilla:
pairwise.t.test(digitsymbol_1, education, p.adj="bonf") # bonferroni-korjaus
pairwise.t.test(digitsymbol_1, education, p.adj="holm") # holm-korjaus

# Tukeyn post hoc -testi:
TukeyHSD(A1)

# ---

# 4.3 Harjoituksia

# Kysymys 6

# Vaikuttaako hormonitaso merkkitestin perussuoriutumiseen eri tavoin eri koulutusryhmissä?

anova1 <- aov(digitsymbol_1~ht_group*education)
summary(anova1)

# --> ei vaikuta, koska yhteisvaikutus ei ole merkitsevä (F(2,41)=1.24, p>.05)
# --> koulutustaso ja hormonihoito eivät myöskään vaikuta merkkitestin perussuoriutumiseen yksinään (ei-merkitsevät päävaikutukset)

# -

# Kysymys 7

# Vaikuttavatko koulutustaso ja ikä yhdessä merkkitestin perussuoriutumiseen?

anova2 <- aov(digitsymbol_1 ~ age*education)
summary(anova2)

# --> eivät vaikuta (yhteisvaikutuksella vain merkitsevää lähestyvä trendi) (F(2,41)=2.73, p>.05)
# --> ikä ja koulutustaso eivät myöskään vaikuta merkkitestin perussuoriutumiseen yksinään (ei-merkitsevät päävaikutukset)

# -

# Kysymys 8

# Iän, koulutustason ja hormonihoidon vaikutuksen Bentonin testin perussuoriutumisessa?

anova3 <- aov(bentonerror_1 ~ age*education*ht_group)
summary(anova3)

# --> koulutustason ja hormonihoidon yhteysvaikutus on merkitsevä (F(2,35)=3.46, p<.05) 
# --> koulutustason ja iän yhteisvaikutus on merkitsevä (F(2,35)=3.66, p<.05)
# --> vain hormonitason päävaikutus on merkitsevä (F(1,35)=10.21, p<.01)

# tulosten visualisointi:

boxplot(bentonerror_1 ~ ht_group, xlab="Hormonihoitoryhmät", ylab="Bentonin testin virhepisteet")
# --> hormonihoitoa saavat osallistujat tekevät kontrolliryhmää vähemmän virheitä Bentonin testissä

interaction.plot(education,age,bentonerror_1, xlab="Koulutustaso", ylab="Benton testin virhepisteet")
# --> korkeampi koulutustaso parantaa suoriutumista nuoremmilla osallistujilla, vanhemmilla osallistuja trendi on päinvastainen

interaction.plot(education,ht_group, bentonerror_1)
# --> perusastetta korkeampi koulutustaso parantaa suoriutumista hormonihoitoa saavien ryhmässä, kontrolliryhmässä vaikutus päinvastainen mutta pienempi

# -----

# 5. Soveltavia harjoituksia

# Ladataan aineisto:

naming <- read.csv('http://bit.ly/PSY204_naming_wide')

# Tarkastellaan aineistoa:

head(naming)
summary(naming)
dim(naming)

# Faktorit sex ja reading_time ovat merkkijonoja, ne täytyy koodata faktoreiksi:
naming$sex <- factor(naming$sex)
naming$reading_time <- factor(naming$reading_time)

# Ei oudosti koodattujan puuttuvia arvoja (viime viikon puuttuvat arvot on poistettu tästä datasta)

# Lukunopeusmuuttuja on jaettu kahteen sarakkeeseen (ms.regular ja ms.exception), eli yhden
# osallistujan kaikki havainnot ovat samalla rivillä.

detach()			# poistetaan kaikkien aiempien tietokehysten kiinnitys
attach(naming)		# kiinnitetään uusi tietokehys analyyseja varten

# -

# Kysymys 9

t.test(iq, mu=100)

# --> älykkyyden otoskeskiarvo ei eroa populaation keskiarvosta (t(997)=0, p=1).
# --> otoksemme on siis edustava

# -

# Kysymys 10

t.test(iq ~ sex)

# --> älykkyydessä ei ole sukupuolieroja (t(961)=1.07, p=.29).

# -

# Kysymys 11

t.test(hrs ~ sex)

# --> viikossa lukemiseen käytetyssä ajassa ei ole sukupuolieroja (t(972)=0.29, p=.77).

# -

# Kysymys 12

anova.1 <- aov(iq ~ reading_time)
summary(anova.1)

# --> kolmen lukuryhmän välillä ei ole eroa älykkyydessä (F(2,995)=0.03, p=.97.

# -

# Kysymys 13

t.test(ms.regular, ms.exception, paired=T)
mean(ms.regular)
mean(ms.exception)
boxplot(ms.regular, ms.exception, xlab="Word type (1=regular, 2=exceptional))", ylab="Time (ms)")

# --> tavallisten sanojen lukeminen on harvinaisia sanoja nopeampaa (t(997)=-68.8, p<.001).

# -

# Kysymys 14

# Tavalliset sanat: 

anova.2 <- aov(ms.regular ~ reading_time)
summary(anova.2)
boxplot(ms.regular ~ reading_time)	

# tutkitaan eroja tarkemmin post hoc -testillä:
pairwise.t.test(ms.regular, reading_time, p.adj="bonf")
tapply(ms.regular, reading_time, mean)
tapply(ms.regular, reading_time, sd)

# --> lukuryhmien välillä on eroja tavallisten sanojen lukunopeudessa (F(2,995)=3.52, p<.05).
# --> post hoc -vertailut parittaisilla t-testeillä (Bonferroni-korjausta käyttäen) osoittavat,
# että ero johtuu merkitsevästä erosta korkeimman lukuajan ryhmän (keskiarvo 993 ms, kh 70.39) 
# ja matalimman lukuajan ryhmän välillä (keskiarvo 1009 ms, kh 79).

# Harvinaiset sanat:

anova.3 <- aov(ms.exception ~ reading_time)
summary(anova.3)
boxplot(ms.exception ~ reading_time)

# tutkitaan eroja tarkemmin post hoc -testillä:
pairwise.t.test(ms.exception, reading_time, p.adj="bonf")
tapply(ms.exception, reading_time, mean)
tapply(ms.exception, reading_time, sd)

# --> lukuryhmien välillä on eroja tavallisten sanojen lukunopeudessa (F(2,995)=38.99, p<.001).
# --> post hoc -vertailut parittaisilla t-testeillä (Bonferroni-korjausta käyttäen) osoittavat,
# että kaikki ryhmät eroavat toisistaan: korkeimman lukuajan ryhmässä sanojen lukeminen on 
# nopeinta (keskiarvo 1173 ms, kh 76), keskitason ryhmässä keskitasoa (keskiarvo 1203, kh 79) ja
# matalimman lukuajan ryhmässä hitainta (keskiarvo 1228, kh 76).

# -

# Kysymys 15

# Tavalliset sanat:

anova.4 <- aov(ms.regular ~ sex * reading_time) 
summary(anova.4)

# interaktiokuvio:
interaction.plot(reading_time, sex, ms.regular)

# --> lukemiseen käytetyn ajan vaikutus tavallisten sanojen lukunopeuteen ei eroa miehillä ja naisilla

# Harvinaiset sanat:

anova.5 <- aov(ms.exception ~ sex * reading_time) 
summary(anova.5)

# interaktiokuvio:
interaction.plot(reading_time, sex, ms.exception)

t.test(ms.exception ~ sex)
boxplot(ms.exception ~ sex)

# # --> lukemiseen käytetyn ajan vaikutus harvinaisten sanojen lukunopeuteen ei eroa miehillä ja naisilla



# -----

