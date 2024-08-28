# PSY.204
# Harjoitusmoniste R1.2
# Aineiston valmistelu tilastollista testausta varten
# Mallivastaukset
# HS 27.8.2024

# -----

# 1 Aineiston lataaminen
# Asetetaan työskentelykansio:
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Viikko 1 - Johdatus')

# Ladataan aineisto:
naming <- read.csv("naming_task.csv", sep=";")

# Kysymys 1:
?read.csv
# sep=";" kertoo R:lle, että sarakkeet on eroteltu puolipisteellä

# Kysymys 2:
names(naming)
# iq, hrs, word.type, ms, male

# Kysymys 3: 
summary(naming)
# Aineistossa on 1000 havaintoa (riviä) ja 7 muuttujaa (saraketta).
# id - jatkuva muuttuja, vaihteluväli 1-1000
# iq - jatkuva muuttuja, vaihteluväli 45.13-999 (???)
# hrs - jatkuva muuttuja, vaihteluväli 2.4-5.5
# male - jatkuva muuttuja (???), vaihteluväli 0-1
# ses - merkkijono
# regular - jatkuva muuttuja, vaihteluväli 701-1263.1
# exception - jatkuva muuttuja, vaihteluväli 942-1452
# Huomataan, että iq-muuttujassa on epäilyttävän isoja arvoja.
# Huomataan, että id- ja male-muuttujat on koodattu jatkuvana muuttujana.
# Huomataan, että ses-muuttuja on merkkijono.

# ---

# 2 Muuttujien valmistelu

# Kysymys 4:
summary(naming$hrs) # jatkuva, numeerinen muuttuja
summary(naming$ses) # kategorinen muuttuja
# hrs-muuttuja on numeerinen muuttuja, joten summary tulostaa sille tunnuslukuja
# ses-muuttuja on kategorinen muuttuja, jonka tietotyypin R lukee merkkijonoksi

# Kysymys 5: 
class(naming$hrs) # numeerinen (numeric)
class(naming$ses) # merkkijono (character)

# Kysymys 6:
summary(naming)
# male-muuttujan yhteenveto viittaa jatkuvaan muuttujaan, vaikka se vaikuttaa olevan kategorinen muuttuja (sukupuoli)
# tämä täytyy korjata, jotta tulevat analyysit toimivat oikein
class(naming$male)
# tarkistettiin vielä muuttujan tietotyyppi: male on tosiaan koodattu numeerisena muuttujana
# Tämä johtuu siitä, että aineistossa sukupuoli on koodattu numerolla (0=nainen, 1=mies). Kun luit aineiston read.csv-komennolla,
# R arvasi, että numerot ovat numeerisia muuttujia.
# R:lle pitää erikseen kertoa, että 'male' on faktori.

# Kysymys 7: 
naming$ses <- factor(naming$ses)
naming$male <- factor(naming$male)
naming$id <- factor(naming$id)
summary(naming) # tarkistus, nyt näyttää oikealta
class(naming$male) # myös muuttujan tietotyyppi on oikein 
levels(naming$male) # funktiolla levels voi tarkistaa muuttujan luokat

# Kysymys 8:
# lisätään faktorille luokat:
# käytän luokille nimiä 'female' and 'male', mutta nimi voi olla mitä tahansa muutakin informatiivista
# tallennetaan luokat uuteen muuttujaan 'sex':
naming$sex[which(naming$male==1)] <- 'male'
naming$sex[which(naming$male==0)] <- 'female'
naming$sex <- factor(naming$sex)
# yllä lisättiin sarake 'sex', jolle annettiin heti arvo 'male' riveille, joissa muuttuja 'male' on 1...
# ... ja arvo 'female' riveille, jossa 'male' on 0
# lopuksi muutettiin uusi muuttuja faktoriksi
# tarkistetaan mitä tietokehykselle tapahtui:
head(naming)				
# Nyt tietokehyksessä on uusi sarake, jossa sukupuoli on koodattu tekstinä
# Huomaa myös nämä muutokset:
summary(naming$sex)
levels(naming$sex)
# Koska muuttujat 'male' ja 'sex' sisältävät saman infomaation, voidaan toinen niistä poistaa:
naming$male <- NULL
summary(naming)

# ---

# 3 Poikkeavat ja puuttuvat havainnot

# Kysymys 9:
summary(naming)			# funktio 'summary' on taas tarpeen
# Tarkastellaan arvoja tarkemmin.
# Ovatko muuttujien minimi- ja maksimiarvot järkeviä?
# Kaikki on muuten ok, mutta näyttää siltä, että muuttuja 'iq' saa kummallisia arvoja...
# Jos mietit älykkyyttä, se ei koskaan saa 200 suurempia arvoja
# Mutta muuttujassa 'iq' on tätä korkeampia arvoja. Tarkastellaan sitä tarkemmin:
max(naming$iq) 			# tulostaa muuttujan 'iq' maksimiarvon
boxplot(naming$iq)	# muuttujan jakauman tarkastelu laatikkokuviolla
hist(naming$iq)     # muuttujan jakauman tarkastelu histogrammilla

# Muuttujan 'iq' arvo 999 on joko kirjoitusvirhe tai yritys koodata puuttuva arvo.
# Koodataan nämä arvot puuttuvina arvoina.
# R:ssä puuttuvat arvot koodataan NA:lla.

# Kysymys 10
which(naming$iq==999)		# paikannetaan rivit, joilla 'iq' saa arvon 999
# Riveillä 33 ja 278 'iq' saa arvon 999.
# Koodataan nämä arvot puuttuvina arvoina (NA):
naming$iq[which(naming$iq==999)] <- NA
# Tarkastellaan, mitä näille havainnoille tapahtui:
naming[c(33, 278),]
# Näillä havainnoilla muuttuja 'iq' saa nyt koodin NA, muut muuttujat eivät muutu.
# Tarkistetaan, mitä muuttujan 'iq' yhteenvedolle tapahtui:
summary(naming)
# Funktio summary näyttää jokaisen muuttujan puuttuvat arvot (NA)

## Kysymys 11
nrow(naming)			# tarkastetaan havaintojen määrä aineistossa
length(unique(naming$id)) # tarkistetaan uniikkien id-arvojen lukumäärä
# Havaintoja on 1000.
sum(complete.cases(naming)) # tarkistetaan kokonaisen datan määrä
sum(is.na(naming)) # sama, mutta tarkistetaan osallistujat, joilta puuttuu dataa

# Aineistossamme on siis kokonainen data 998 osallistujalta. Kahdelta osallistujalta puuttuu 'iq'.

# ---

# 4 Muuttujien lisääminen ja poistaminen

# Kysymys 12:
# Luodaan muuttujan 'reading.group' ja annetaan sille arvot lukemiseen
# viikossa käytettävään aikaan perustuen:
naming$reading.group <- 'low'
naming$reading.group[which(naming$hrs > 3.5)] <- 'medium'
naming$reading.group[which(naming$hrs > 4.5)] <- 'high'
# Lopuksi muutetaan vielä uusi muuttuja faktoriksi:
naming$reading.group <- factor(naming$reading.group)

# Kysymys 13:
naming$ses <- NULL # poistetaan turha sarake

# Kysymys 14:
# Tallennetaan erikseen miesten aineisto:
naming_male <- naming[which(naming$sex=='male'),]
naming_female <- naming[which(naming$sex=='female'),]

# -----

# 5 Jakaumien tarkastelu

# Kysymys 15:
hist(naming$regular)
hist(naming$exception)
