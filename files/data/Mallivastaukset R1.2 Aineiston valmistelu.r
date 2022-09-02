# PSY.204
# Harjoitusmoniste R1.2
# Aineiston valmistelu tilastollista testausta varten
# Esimerkkivastaukset / HS 29.8.2022

# -----

# 1 Aineiston lataaminen

# Asetetaan työskentelykansio:
setwd('C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/R-harkat')

# Ladataan aineisto:
naming <- read.csv("https://bit.ly/PSY204_naming", header=TRUE, sep="\t")

# Kysymys 1:
?read.csv
# header=TRUE kertoo R:lle, että muuttujien nimet ovat tiedoston ensimmäisellä rivillä.
# sep="\t" kertoo R:lle, että rivit on eroteltu tabulaattorilla

# Kysymys 2:
names(naming)
# iq, hrs, word.type, ms, male

# Kysymys 3: 
summary(naming)
# Aineistossa on 2000 havaintoa (riviä) ja 5 muuttujaa (saraketta).
# iq - jatkuva muuttuja, vaihteluväli 45.13-999 (???)
# hrs - jatkuva muuttuja, vaihteluväli 2.4-5.5
# word.type - kategorinen muuttuja, luokat exception ja regular
# ms - jatkuva muuttuja, vaihteluväli 701-1452
# male - jatkuva muuttuja (???), vaihteluväli 0-1
# Huomataan, että iq-muuttujassa on epäilyttävän isoja arvoja.
# Huomataan, että male-muuttuja on koodattu jatkuvana muuttujana.

# ---

# 2 Faktorit

# Kysymys 4:
summary(naming$hrs) # jatkuva, numeerinen muuttuja
summary(naming$word.type) # faktori
# hrs-muuttuja on numeerinen muuttuja, joten summary tulostaa sille tunnuslukuja
# word.type-muuttuja on kategorinen muuttuja eli faktori, joten summary tulostaa sille kategorioiden frekvenssit
# muistutuksena: kun luit aineiston käyttäen read.csv-komentoa, R arvasi automaattisesti, että numerot (esim. hrs) ovat numeerisia 
# muuttujia ja merkkijonot (esim. word.type) faktoreita

# Kysymys 5: 
class(naming$hrs) # numeerinen
class(naming$word.type) # faktori

# Kysymys 6:
summary(naming)
# male-muuttujan yhteenveto viittaa jatkuvaan muuttujaan, vaikka se vaikuttaa olevan kategorinen muuttuja (sukupuoli)
# tämä täytyy korjata, jotta tulevat analyysit toimivat oikein
class(naming$male)
# tarkistettiin vielä muuttujan tietotyyppi: male on tosiaan koodattu numeerisena muuttujana
# Tämä johtuu siitä, että aineistossa sukupuoli on koodattu numerolla (0=nainen, 1=mies). Kun luit aineiston read.csv-komennolla,
# R jälleen arvasi, että numerot ovat numeerisia muuttujia.
# R:lle pitää erikseen kertoa, että 'male' on faktori.

# Kysymys 7: 
naming$male <- factor(naming$male)
summary(naming) # tarkistus, nyt näyttää oikealta
class(naming$male) # myös muuttujan tietotyyppi on oikein

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
head(naming$sex)
# Muuttujan 'sex' arvojen tulostaminen esim. funktiolla 'head' näyttää myös faktorin luokat ('Levels')
# Näin tapahtuu myös aineiston toiselle faktorille eli muuttujalle 'word.type'
head(naming$word.type)
# Koska muuttujat 'male' ja 'sex' sisältävät saman infomaation, voidaan toinen niistä poistaa:
naming$male <- NULL
summary(naming)

# Kysymys 9:
summary(naming)			# funktio 'summary' on taas tarpeen
# Tarkastellaan arvoja tarkemmin.
# Ovatko muuttujien minimi- ja maksimiarvot järkeviä?
# Kaikki on muuten ok, mutta näyttää siltä, että muuttuja 'iq' saa kummallisia arvoja...
# Jos mietit älykkyyttä, se ei voi käytännössä koskaan saa 200 suurempia arvoja
# Mutta muuttujassa 'iq' on tätä korkeampia arvoja. Tarkastellaan sitä tarkemmin:
max(naming$iq) 			# tulostaa muuttujan 'iq' maksimiarvon
boxplot(naming$iq)	# voidaan myös tulostaa muuttujan 'iq' jakauman kuva: joukossa on havaintoja, jotka saavat arvon 999 
# Lisää kuvioiden tekemisestä ensi viikolla!
# Muuttujan 'iq' arvo 999 on joko kirjoitusvirhe tai yritys koodata puuttuva arvo.
# Koodataan nämä arvot puuttuvina arvoina.
# R:ssä puuttuvat arvot koodataan NA:lla.

# Kysymys 10
which(naming$iq==999)		# paikannetaan rivit, joilla 'iq' saa arvon 999
# Riveillä 33, 278, 1033, 1278 'iq' saa arvon 999.
# Koodataan nämä arvot puuttuvina arvoina (NA):
naming$iq[which(naming$iq==999)] <- NA
# Tarkastellaan, mitä näille havainnoille tapahtui:
naming[c(33, 278, 1033, 278),]
# Näillä havainnoilla muuttuja 'iq' saa nyt koodin NA, muut muuttujat eivät muutu.
# Tarkistetaan, mitä muuttujan 'iq' yhteenvedolle tapahtui:
summary(naming)
# Funktio summary näyttää jokaisen muuttujan puuttuvat arvot (NA)

## Kysymys 11
# Yleensä tunnet aineistosi, joten tämä on selvää, mutta vieraiden aineistotaulukoiden kohdalla et välttämättä tiedä, miten aineisto kerättiin.
nrow(naming)			# tarkastetaan havaintojen määrä aineistossa
summary(naming)			# käytetään taas summary-funktiota apuna
# Havaintoja on 2000.
# Mutta huomaa, että muuttuja 'word.type' sisältää kaksi koetilannetta.
# Vaikuttaa siltä, että kaikki osallistujat osallistuivat molempiin koetilanteisiin (expection ja regular).
# Voiko siis olla, että jokainen osallistuja on aineistossa kaksi kertaa?
# Tämä täytyy tarkistaa. Vertaillaan ensimmäistä 10 ja "toista" 10 havaintoa:
naming[1:10,]
naming[1001:1010,]
# Huomaa, että taustamuuttujat 'iq', 'hrs', and 'sex' saavat samat arvot riveillä 1:10 ja 1001:1010
# ... on epätodennäköistä, että tämä on sattumaa. 
# Ensimmäiset 1000 riviä ja toiset 1000 riviä ovat samojen osallistujien havainnot kahdessa eri koetilanteessa.
# Ainoa ero ovat koetilanteet ('word.type') ja koetilanteeseen liittyvät arvot ('ms').
# Huomataan, että aineistossa onkin vain 1000 osallistujaa.
# Näistä kahdella on puuttuvia arvoja (huomaa, että puuttuvat arvotkin koodattiin kahdesti!).
# Voit myös tarkistaa puuttuvat arvot käyttämällä funktiota 'is.na':
naming[is.na(naming$iq),]
# Aineistossamme on siis kokonainen data 998 osallistujalta.

# -----

# 4 Otosten poiminta

# Kysymys 12
naming$id <- factor(rep(1:1000, times=2)) # lisätään faktori 'id' jossa on juokseva numerointi osallistujille
head(naming)					# tarkistetaan tuloksia
naming$id					 	
# Osallistujanumerot luotiin perustuen havaintoomme, että jokainen 1000 osallistujasta on aineistossa kahdesti.

## Kysymys 13
?sample						# tutkitaan, miten funktio 'sample' toimii
my.sample <- sample(1000,100)			# poimitaan satunnaisotos
my.sample						
my.sample <- sort(my.sample)			# järjestetään satunnaisesti poimitut numerot (see ?sort)
my.sample

# Kysymys 14
# Kokeillaan ensin näin:
naming_sample <- naming[my.sample,]		# tässä poimitaan vain rivit aineistomme perusteella
# Ok, tämä toimi, mutta oliko se oikein?
summary(naming_sample)
dim(naming_sample)
# Hups - tämä toimii joskus, mutta tässä tapauksessa ainestossamme on samat osallistujat kahdesti.
# Poimimamme otos sisältää vain rivejä ensimmäisiltä 1000 havainnolta
# ... joten aineistosta tippuu pois tavallisten sanojen koetilanne
# Otoksen rivien määrä on 100, mutta sen pitäisi sisältää 200 riviä, eli osallistujat kahdesti.
# Kokeillaan toista tapaa:
naming_sample <- naming[c(my.sample, (my.sample+1000)),] # poimimme rivit väliltä 1-1000 JA samat rivit väliltä 1001-2000
summary(naming_sample)
dim(naming_sample)
# Halutessasi voit myös käyttää %in%-operaattoria. Katso, miten se toimii: ?%in%
naming_sample <- naming[naming$id %in% my.sample,]
summary(naming_sample)
dim(naming_sample)

# -----

# Bonus: naming-aineiston uudelleenjärjestäminen

# Yllä oleva naming-aineisto on ns. pitkässä muodossa, eli kaikki koetilanteet ovat aineistossa omilla riveillään.
# Aineiston voi myös kääntää leveään muotoon, jolloin jokaiselle osallistujalle on vain yksi rivi ja eri koetilanteiden tulokset 
# ... ovat omissa sarakkeissaan.

# Siivousapuna toimii 'tidyr'-kirjasto:
library(tidyr)

# Funktiolla 'pivot_wider' saadaan aineisto käännettyä leveään muotoon:
naming_wide <- pivot_wider(naming, names_from="word.type", values_from="ms")
