# PSY204
# R-harjoitusmoniste R2.1
# Mallivastaukset
# Heini Saarimäki 4.9.2024

# -----

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Harjoitukset/')

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:
deprivation <- read.csv('https://bit.ly/PSY204_deprivation')

# Aineiston tarkastelu
head(deprivation)		# 10 muuttujaa
dim(deprivation)		# 47 riviä (eli 47 osallistujaa, yksi per rivi)
summary(deprivation)

# Kategorisia muuttujia ID, age, education ja ht_group ei vielä ole koodattu faktoreina
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

# Kun olet valmistellut aineiston, sen voi kiinnittää:

attach(deprivation)

# ---

# 2. Aineiston kuvailu

# Tarkistetaan ryhmien frekvenssit kaikissa kategorisissa muuttujissa:

summary(deprivation[2:4])

# Tarkastellaan vielä frekvenssejä ristiintaulukoinnin avulla:

table(age, education)
table(age, ht_group)
table(education, ht_group)

# Kuvailevien tulosten taulukko jatkuville muuttujille:

# install.packages('psych') # tarvittaessa asenna kirjasto 'psych'
library('psych')			# ladataan kirjasto 'psych', josta saadaan funktio 'describe' käyttöön
describe(deprivation[5:10])	# otetaan kuvailevat tulokset jatkuville muuttujille, poiminta sarakkeiden numeroiden avulla

# Tallennetaan kuvailevat tulokset mahdollista jatkokäyttöä varten:

deprivation.descriptives <- describe(deprivation[5:10])
write.csv(deprivation.descriptives, "deprivation_tunnusluvut.csv")

# Jatkuvien muuttujien visualisointia:

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

# a) Merkkikoe

# Verrataan otoskeskiarvoa tunnettuun populaation keskiarvoon:

summary(digitsymbol_1)		# otoskeskiarvo 42.23, populaation keskiarvo 40

# Oletukset:

hist(digitsymbol_1)
qqnorm(digitsymbol_1)
qqline(digitsymbol_1, col="red")
shapiro.test(digitsymbol_1)

# Onko ero tilastollisesti merkitsevä?
# Käytetään yhden otoksen t-testiä:

t.test(digitsymbol_1, mu=40)

# --> Otoskeskiarvo ei poikkea tilastollisesti merkitsevästi populaation odotusarvosta (t(46)=1.33, p=.19)

# b) Bentonin testi

# Otoskeskiarvo:
summary(bentonerror_1)

# Oletukset:
hist(bentonerror_1)
qqnorm(bentonerror_1)
qqline(bentonerror_1, col="red")
shapiro.test(bentonerror_1)
ks.test(bentonerror_1, pnorm, mean=mean(bentonerror_1, na.rm=T), sd=sd(bentonerror_1, na.rm=T))

# Yhden otoksen t-testi:
t.test(bentonerror_1, mu=5)

# --> Otoskeskiarvo poikkeaa tilastollisesti merkitsevästi populaation odotusarvosta (t(46)=2.95, p=.005)


# ---

# 3.2 Riippumattomien otosten t-testi

# a) Merkkikoe

#Keski- ja hajontaluvut erikseen eri ryhmille:
tapply(digitsymbol_1, age, mean, na.rm=T)	# keskiarvo (mean)
tapply(digitsymbol_1, age, sd, na.rm=T)		# keskihajonta (sd)

# Oletukset
# Histogrammit erikseen nuorille ja vanhoille:
par(mfrow=c(1,2))
hist(subset(deprivation, age == 1)$digitsymbol_1, 
     main = "Nuoret", 
     xlab = "Merkkikoe", ylab="Frekvenssi")
hist(subset(deprivation, age == 2)$digitsymbol_1, 
     main = "Vanhat", 
     xlab = "Merkkikoe", ylab="Frekvenssi")

# Shapiro-Wilk
shapiro.test(subset(deprivation, age == 1)$digitsymbol_1)
shapiro.test(subset(deprivation, age == 2)$digitsymbol_1)

# Varianssien yhtäsuuruus
var.test(digitsymbol_1~age)

# T-testi ikäryhmien vertailemiseksi merkkitestissä mittaushetkellä 1:

t.test(digitsymbol_1 ~ age)	# kategorinen muuttuja tulee merkin ~ jälkeen

# Efektikoko

library(lsr)
cohensD(digitsymbol_1 ~ age)

# Ryhmien keskiarvot eivät eroa toisistaan (t(35)=0.27, p=.792, d=0.08)

# Näytetään tulokset laatikkokuvion avulla:
boxplot(digitsymbol_1 ~ age, xlab="Ikäryhmä", ylab="Merkkitestin pistemäärä")

# Muokataan kuvaa julkaisukelpoisemmaksi:

library(ggplot2)
ggplot(deprivation, aes(x=age, y=digitsymbol_1, fill=age)) +
  geom_boxplot() +
  labs(y = "Merkkitestin pistemäärä", x = "Ikäryhmä") +
  theme(legend.position = "none")

# b) Bentonin testi

#Keski- ja hajontaluvut erikseen eri ryhmille:
tapply(bentonerror_1, age, mean, na.rm=T)	# keskiarvo (mean)
tapply(bentonerror_1, age, sd, na.rm=T)		# keskihajonta (sd)

# Oletukset

# Histogrammit erikseen nuorille ja vanhoille:
par(mfrow=c(1,2))
hist(subset(deprivation, age == 1)$bentonerror_1, 
     main = "Nuoret", 
     xlab = "Benton", ylab="Frekvenssi")
hist(subset(deprivation, age == 2)$bentonerror_1, 
     main = "Vanhat", 
     xlab = "Benton", ylab="Frekvenssi")

# Shapiro-Wilk
shapiro.test(subset(deprivation, age == 1)$bentonerror_1)
shapiro.test(subset(deprivation, age == 2)$bentonerror_1)

# Varianssien yhtäsuuruus
var.test(bentonerror_1~age)

# T-testi ikäryhmien vertailemiseksi merkkitestissä mittaushetkellä 1:
t.test(bentonerror_1 ~ age, var.equal=T)	# var.equal=T argumentti lisätään, koska varianssit olivat yhtä suuret

# Efektikoko:
cohensD(bentonerror_1 ~ age)

# Ryhmien keskiarvot eivät eroa toisistaan (t(45)=-1.46, p=.152, d=0.426)

# Näytetään tulokset laatikkokuvion avulla:

boxplot(bentonerror_1 ~ age, xlab="Ikäryhmä", ylab="Bentonin testin pistemäärä")

# Muokataan kuvaa julkaisukelpoisemmaksi:

ggplot(deprivation, aes(x=age, y=bentonerror_1, fill=age)) +
  geom_boxplot() +
  labs(y = "Bentonin testin pistemäärä", x = "Ikäryhmä") +
  theme(legend.position = "none")


# ---

# 3.3 Toistettujen mittausten t-testi

# a) Merkkikoe

# Koetilannekohtaiset keskiarvot ja -hajonnat
describe(digitsymbol_1)
describe(digitsymbol_2)

# Oletukset

# Histogrammit
par(mfrow=c(1,2))
hist(digitsymbol_1, 
     main = "Mittauspiste 1", 
     xlab = "Merkkikoe", ylab="Frekvenssi")
hist(digitsymbol_2, 
     main = "Mittauspiste 2", 
     xlab = "Merkkikoe", ylab="Frekvenssi")

# Shapiro-Wilk
shapiro.test(digitsymbol_1)
shapiro.test(digitsymbol_2)

# Toistettujen mittausten t-testi univajeen vaikutuksen testaamiseksi:
t.test(digitsymbol_1, digitsymbol_2, paired=T)

# Efektikoko:
cohensD(digitsymbol_1, digitsymbol_2)

# Mittaushetkien 1 ja 2 välillä ei ole eroa eli univajeella ei ole vaikutusta (t(46)=-0.79, p=0.436, d=0.06).

# Laatikkokuvio:

boxplot(digitsymbol_1, digitsymbol_2, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")

# b) Bentonin testi

# Koetilannekohtaiset keskiarvot ja -hajonnat
describe(bentonerror_1)
describe(bentonerror_2)

# Oletukset

# Histogrammit
par(mfrow=c(1,2))
hist(bentonerror_1, 
     main = "Mittauspiste 1", 
     xlab = "Bentonin testi", ylab="Frekvenssi")
hist(bentonerror_2, 
     main = "Mittauspiste 2", 
     xlab = "Bentonin testi", ylab="Frekvenssi")

# Shapiro-Wilk
shapiro.test(bentonerror_1)
shapiro.test(bentonerror_2) # ei normaalijakautunut

# Wilcoxonin testi univajeen vaikutuksen testaamiseksi:

wilcox.test(bentonerror_1, bentonerror_2, paired=T)

# Merkkikokeen pistemääriä mittauspisteissä 1 ja 2 testattiin Wilcoxonin merkittyjen sijalukujen testillä. 
# Pistemäärät mittauspisteissä 1 ja 2 eivät poikenneet tilastollisesti merkitsevästi toisistaan (𝑉=488.5, p=.086).

# Laatikkokuvio:

boxplot(bentonerror_1, bentonerror_2, ylab="Bentonin testin pistemäärä", xlab="Mittaushetki")


# ---

# 3.4 Harjoituksia

# Kysymys 1:

# Oletukset:
shapiro.test(digitsymbol_1) # muuttuja on normaalijakautunut
var.test(digitsymbol_1 ~ ht_group) # varianssit ovat yhtä suuret

# Riippumattomien otosten t-testi:
t.test(digitsymbol_1 ~ ht_group, var.equal=T)
cohensD(digitsymbol_1 ~ ht_group)

# Ryhmien välillä ei ole eroa (t(45)=-0.17, p=0.86, d=0.05).

# -

# Kysymys 2:

# Mittaushetket 1 ja 3:

# Oletukset:
shapiro.test(digitsymbol_1) # normaalijakautunut
shapiro.test(digitsymbol_3) # normaalijakautunut

# Toistettujen mittausten t-testi:
t.test(digitsymbol_1, digitsymbol_3, paired=TRUE)
cohensD(digitsymbol_1, digitsymbol_3)

# Mittaushetkien 1 ja 3 välillä on tilastollisesti merkitsevä ero (t(46)=-6.9, p<.001, d=0.49).

# Tarkastellaan eroja laatikkokuviolla:

boxplot(digitsymbol_1, digitsymbol_3, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")

# Kuviosta huomataan, että pistemäärät ovat korkeammat mittaushetkellä 3.
# Kokeen aikana tapahtuu todennäköisesti oppimista.

# Mittaushetket 2 ja 3:

# Oletukset:
shapiro.test(digitsymbol_2) # normaalijakautunut
shapiro.test(digitsymbol_3) # normaalijakautunut

# Toistettujen mittausten t-testi:
t.test(digitsymbol_2, digitsymbol_3, paired=TRUE)
cohensD(digitsymbol_2, digitsymbol_3)

# Mittaushetkien 1 ja 3 välillä on tilastollisesti merkitsevä ero (t(46)=-10.6, p<.001, d=0.42).
# Tarkastellaan eroja laatikkokuviolla:

boxplot(digitsymbol_2, digitsymbol_3, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")

# Kuviosta huomataan, että pistemäärät ovat korkeammat mittaushetkellä 3.
# Eli sama todennäköisesti oppimisvaikutuksesta johtuva eroaa näkyy mittaushetkien 2 ja 3 välillä.

# Yhteenvetona voidaan todeta, että merkkitestissä tapahtuu oppimista kokeen aikana.
# On mahdollista, että emme saaneet tilastollisesti merkitsevää univajeen vaikutusta siksi,
# että kokeen aikana tapahtuva oppiminen peitti univajeen vaikutuksen alleen.

# -

# Kysymys 3:

# Mittaushetket 1 ja 3:

# Oletukset:
shapiro.test(bentonerror_1) # normaalijakautunut
shapiro.test(bentonerror_3) # ei normaalijakautunut

# Wilcoxonin testi:
wilcox.test(bentonerror_1, bentonerror_3, paired=TRUE)

# Mittaushetkien 1 ja 3 välillä on tilastollisesti merkitsevä ero (𝑉=840, p=<.001).

# Tarkastellaan eroja laatikkokuviolla:

boxplot(bentonerror_1, bentonerror_3, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")


# Mittaushetket 2 ja 3:

# Oletukset:
shapiro.test(bentonerror_2) # ei normaalijakautunut
shapiro.test(bentonerror_3) # ei normaalijakautunut

# Wilcoxonin testi:
wilcox.test(bentonerror_2, bentonerror_3, paired=TRUE)

# Mittaushetkien 2 ja 3 välillä on tilastollisesti merkitsevä ero (𝑉=769, p=<.001).

# Tarkastellaan eroja laatikkokuviolla:

boxplot(bentonerror_2, bentonerror_3, ylab="Merkkitestin pistemäärä", xlab="Mittaushetki")


# -----
