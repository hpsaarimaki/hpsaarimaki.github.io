# PSY.204
# Harjoitusmoniste R1.2
# Aineiston valmistelu tilastollista testausta varten
# Esimerkkivastaukset / HS 29.8.2022

# -----

# 1 Aineiston lataaminen

# Asetetaan ty�skentelykansio:
setwd('C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/R-harkat')

# Ladataan aineisto:
naming <- read.csv("https://bit.ly/PSY204_naming", header=TRUE, sep="\t")

# Kysymys 1:
?read.csv
# header=TRUE kertoo R:lle, ett� muuttujien nimet ovat tiedoston ensimm�isell� rivill�.
# sep="\t" kertoo R:lle, ett� rivit on eroteltu tabulaattorilla

# Kysymys 2:
names(naming)
# iq, hrs, word.type, ms, male

# Kysymys 3: 
summary(naming)
# Aineistossa on 2000 havaintoa (rivi�) ja 5 muuttujaa (saraketta).
# iq - jatkuva muuttuja, vaihteluv�li 45.13-999 (???)
# hrs - jatkuva muuttuja, vaihteluv�li 2.4-5.5
# word.type - kategorinen muuttuja, luokat exception ja regular
# ms - jatkuva muuttuja, vaihteluv�li 701-1452
# male - jatkuva muuttuja (???), vaihteluv�li 0-1
# Huomataan, ett� iq-muuttujassa on ep�ilytt�v�n isoja arvoja.
# Huomataan, ett� male-muuttuja on koodattu jatkuvana muuttujana.

# ---

# 2 Faktorit

# Kysymys 4:
summary(naming$hrs) # jatkuva, numeerinen muuttuja
summary(naming$word.type) # faktori
# hrs-muuttuja on numeerinen muuttuja, joten summary tulostaa sille tunnuslukuja
# word.type-muuttuja on kategorinen muuttuja eli faktori, joten summary tulostaa sille kategorioiden frekvenssit
# muistutuksena: kun luit aineiston k�ytt�en read.csv-komentoa, R arvasi automaattisesti, ett� numerot (esim. hrs) ovat numeerisia 
# muuttujia ja merkkijonot (esim. word.type) faktoreita

# Kysymys 5: 
class(naming$hrs) # numeerinen
class(naming$word.type) # faktori

# Kysymys 6:
summary(naming)
# male-muuttujan yhteenveto viittaa jatkuvaan muuttujaan, vaikka se vaikuttaa olevan kategorinen muuttuja (sukupuoli)
# t�m� t�ytyy korjata, jotta tulevat analyysit toimivat oikein
class(naming$male)
# tarkistettiin viel� muuttujan tietotyyppi: male on tosiaan koodattu numeerisena muuttujana
# T�m� johtuu siit�, ett� aineistossa sukupuoli on koodattu numerolla (0=nainen, 1=mies). Kun luit aineiston read.csv-komennolla,
# R j�lleen arvasi, ett� numerot ovat numeerisia muuttujia.
# R:lle pit�� erikseen kertoa, ett� 'male' on faktori.

# Kysymys 7: 
naming$male <- factor(naming$male)
summary(naming) # tarkistus, nyt n�ytt�� oikealta
class(naming$male) # my�s muuttujan tietotyyppi on oikein

# Kysymys 8:
# lis�t��n faktorille luokat:
# k�yt�n luokille nimi� 'female' and 'male', mutta nimi voi olla mit� tahansa muutakin informatiivista
# tallennetaan luokat uuteen muuttujaan 'sex':
naming$sex[which(naming$male==1)] <- 'male'
naming$sex[which(naming$male==0)] <- 'female'
naming$sex <- factor(naming$sex)
# yll� lis�ttiin sarake 'sex', jolle annettiin heti arvo 'male' riveille, joissa muuttuja 'male' on 1...
# ... ja arvo 'female' riveille, jossa 'male' on 0
# lopuksi muutettiin uusi muuttuja faktoriksi
# tarkistetaan mit� tietokehykselle tapahtui:
head(naming)				
# Nyt tietokehyksess� on uusi sarake, jossa sukupuoli on koodattu tekstin�
# Huomaa my�s n�m� muutokset:
summary(naming$sex)
head(naming$sex)
# Muuttujan 'sex' arvojen tulostaminen esim. funktiolla 'head' n�ytt�� my�s faktorin luokat ('Levels')
# N�in tapahtuu my�s aineiston toiselle faktorille eli muuttujalle 'word.type'
head(naming$word.type)
# Koska muuttujat 'male' ja 'sex' sis�lt�v�t saman infomaation, voidaan toinen niist� poistaa:
naming$male <- NULL
summary(naming)

# Kysymys 9:
summary(naming)			# funktio 'summary' on taas tarpeen
# Tarkastellaan arvoja tarkemmin.
# Ovatko muuttujien minimi- ja maksimiarvot j�rkevi�?
# Kaikki on muuten ok, mutta n�ytt�� silt�, ett� muuttuja 'iq' saa kummallisia arvoja...
# Jos mietit �lykkyytt�, se ei voi k�yt�nn�ss� koskaan saa 200 suurempia arvoja
# Mutta muuttujassa 'iq' on t�t� korkeampia arvoja. Tarkastellaan sit� tarkemmin:
max(naming$iq) 			# tulostaa muuttujan 'iq' maksimiarvon
boxplot(naming$iq)	# voidaan my�s tulostaa muuttujan 'iq' jakauman kuva: joukossa on havaintoja, jotka saavat arvon 999 
# Lis�� kuvioiden tekemisest� ensi viikolla!
# Muuttujan 'iq' arvo 999 on joko kirjoitusvirhe tai yritys koodata puuttuva arvo.
# Koodataan n�m� arvot puuttuvina arvoina.
# R:ss� puuttuvat arvot koodataan NA:lla.

# Kysymys 10
which(naming$iq==999)		# paikannetaan rivit, joilla 'iq' saa arvon 999
# Riveill� 33, 278, 1033, 1278 'iq' saa arvon 999.
# Koodataan n�m� arvot puuttuvina arvoina (NA):
naming$iq[which(naming$iq==999)] <- NA
# Tarkastellaan, mit� n�ille havainnoille tapahtui:
naming[c(33, 278, 1033, 278),]
# N�ill� havainnoilla muuttuja 'iq' saa nyt koodin NA, muut muuttujat eiv�t muutu.
# Tarkistetaan, mit� muuttujan 'iq' yhteenvedolle tapahtui:
summary(naming)
# Funktio summary n�ytt�� jokaisen muuttujan puuttuvat arvot (NA)

## Kysymys 11
# Yleens� tunnet aineistosi, joten t�m� on selv��, mutta vieraiden aineistotaulukoiden kohdalla et v�ltt�m�tt� tied�, miten aineisto ker�ttiin.
nrow(naming)			# tarkastetaan havaintojen m��r� aineistossa
summary(naming)			# k�ytet��n taas summary-funktiota apuna
# Havaintoja on 2000.
# Mutta huomaa, ett� muuttuja 'word.type' sis�lt�� kaksi koetilannetta.
# Vaikuttaa silt�, ett� kaikki osallistujat osallistuivat molempiin koetilanteisiin (expection ja regular).
# Voiko siis olla, ett� jokainen osallistuja on aineistossa kaksi kertaa?
# T�m� t�ytyy tarkistaa. Vertaillaan ensimm�ist� 10 ja "toista" 10 havaintoa:
naming[1:10,]
naming[1001:1010,]
# Huomaa, ett� taustamuuttujat 'iq', 'hrs', and 'sex' saavat samat arvot riveill� 1:10 ja 1001:1010
# ... on ep�todenn�k�ist�, ett� t�m� on sattumaa. 
# Ensimm�iset 1000 rivi� ja toiset 1000 rivi� ovat samojen osallistujien havainnot kahdessa eri koetilanteessa.
# Ainoa ero ovat koetilanteet ('word.type') ja koetilanteeseen liittyv�t arvot ('ms').
# Huomataan, ett� aineistossa onkin vain 1000 osallistujaa.
# N�ist� kahdella on puuttuvia arvoja (huomaa, ett� puuttuvat arvotkin koodattiin kahdesti!).
# Voit my�s tarkistaa puuttuvat arvot k�ytt�m�ll� funktiota 'is.na':
naming[is.na(naming$iq),]
# Aineistossamme on siis kokonainen data 998 osallistujalta.

# -----

# 4 Otosten poiminta

# Kysymys 12
naming$id <- factor(rep(1:1000, times=2)) # lis�t��n faktori 'id' jossa on juokseva numerointi osallistujille
head(naming)					# tarkistetaan tuloksia
naming$id					 	
# Osallistujanumerot luotiin perustuen havaintoomme, ett� jokainen 1000 osallistujasta on aineistossa kahdesti.

## Kysymys 13
?sample						# tutkitaan, miten funktio 'sample' toimii
my.sample <- sample(1000,100)			# poimitaan satunnaisotos
my.sample						
my.sample <- sort(my.sample)			# j�rjestet��n satunnaisesti poimitut numerot (see ?sort)
my.sample

# Kysymys 14
# Kokeillaan ensin n�in:
naming_sample <- naming[my.sample,]		# t�ss� poimitaan vain rivit aineistomme perusteella
# Ok, t�m� toimi, mutta oliko se oikein?
summary(naming_sample)
dim(naming_sample)
# Hups - t�m� toimii joskus, mutta t�ss� tapauksessa ainestossamme on samat osallistujat kahdesti.
# Poimimamme otos sis�lt�� vain rivej� ensimm�isilt� 1000 havainnolta
# ... joten aineistosta tippuu pois tavallisten sanojen koetilanne
# Otoksen rivien m��r� on 100, mutta sen pit�isi sis�lt�� 200 rivi�, eli osallistujat kahdesti.
# Kokeillaan toista tapaa:
naming_sample <- naming[c(my.sample, (my.sample+1000)),] # poimimme rivit v�lilt� 1-1000 JA samat rivit v�lilt� 1001-2000
summary(naming_sample)
dim(naming_sample)
# Halutessasi voit my�s k�ytt�� %in%-operaattoria. Katso, miten se toimii: ?%in%
naming_sample <- naming[naming$id %in% my.sample,]
summary(naming_sample)
dim(naming_sample)

# -----

# Bonus: naming-aineiston uudelleenj�rjest�minen

# Yll� oleva naming-aineisto on ns. pitk�ss� muodossa, eli kaikki koetilanteet ovat aineistossa omilla riveill��n.
# Aineiston voi my�s k��nt�� leve��n muotoon, jolloin jokaiselle osallistujalle on vain yksi rivi ja eri koetilanteiden tulokset 
# ... ovat omissa sarakkeissaan.

# Siivousapuna toimii 'tidyr'-kirjasto:
library(tidyr)

# Funktiolla 'pivot_wider' saadaan aineisto k��nnetty� leve��n muotoon:
naming_wide <- pivot_wider(naming, names_from="word.type", values_from="ms")
