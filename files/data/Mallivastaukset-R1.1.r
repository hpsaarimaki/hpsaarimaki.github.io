# R-harjoitusmoniste R1.1:
# R:n perusteet
# PSY204 syksy 2023
# Mallivastaukset
# Heini Saarimäki 11.8.2023

# -----

# 1. Perusteita

# Kysymys 1
10+1
# 11

# Kysymys 2
10-1
10*2
5/2
# toimivat kuten oletin

# Kysymys 3
10+1/2	# 10.5
(10+1)/2	# 5.5
# aritmeettiset operaatiot suoritetaan vasemmalta oikealle, ensin * ja /, sitten + ja -
# järjestystä voi vaihtaa sulkeilla

# Kysymys 4
x <- 1
x
# R tulostaa muuttujalle tallennetun arvon

# Kysymys 5
y <- (20/5)+100
z <- x+y
z		# tulos on 105

# Kysymys 6
100 -> a
a
# nuolen suunnalla on merkitystä - nuolen pitää osoittaa muuttujan nimeen
# huom! ei toimi merkin = kanssa:
100 = a	# virhe!

# -----

# 2. Tietotyypit

# Kysymys 7
c(10,20,30)+1
# tuloksena on vektori, joka sisältää arvot 11, 21 ja 31

# Kysymys 8
mun.vektori <- c(10,20,30)+1
mun.vektori
# tallennettiin muuttujaan nimeltä "mun.vektori"

# Kysymys 9
toinen.vektori <- 40:100
toinen.vektori
# tallennettiin vektori muuttujaan nimeltä "toinen.vektori"

# -----

# 3. Tietorakenteet

# Kysymys 10
vektori1 <- seq(1,20,by=0.5)
vektori1
vektori2 <- seq(1,20,length=5)
vektori2
# argumentilla "by" luodaan lukujono, jossa kasvaa aina yhdellä annetulla arvolla lukujonon alkiosta toiseen
# argumentilla "length" luodaan halutun pituinen lukujono

# Kysymys 11
rep(1:5, times=2)
rep(1:5, times=c(3,1,1,5,2))
rep(1:5, each=3)
# argumentti "times" skalaariarvolla toistaa annetun vektorin skalaariarvon verran kertoja
# argumentti "times" vektorilla määrää, kuinka monta kertaa kukin vektorin alkio toistetaan
# argumentti "each" määrää, kuinka monta kertaa peräkkäin kukin vektorin arvo toistetaan

# Kysymys 12
"a":"b"                   # virhe! : ei toimi merkkijonoilla
c("a", "b")               # c toimii
seq("a", "b", by=10)	  # virhe! seq ei toimi merkkijonoilla
rep(c("a","b"), each=3)	  # rep toimii

# Kysymys 13
m <- 1:10
n <- seq(2,20, by=2)
matrix(m, nrow=2, ncol=5)
matrix(m, nrow=2, ncol=5, byrow=TRUE)
cbind(m,n)
rbind(m,n)
# matrix luo matriisin annetuista arvoista, matriisi täytetään oletusarvoisesti sarakkeittan 
# "cbind" ja "rbind" sitovat vektorit yhteen sarakkeiksi ja riveiksi

# Kysymys 14
AgeWeight <- read.csv("https://bit.ly/PSY204_ageweight")
head(AgeWeight) 
# MALE saa arvoja 0 ja 1, ilmeisesti MALE = 1 koodaa miestä

# Kysymys 15
head(AgeWeight)
# 59 vuotta

# Kysymys 16
tail(AgeWeight)
# tail näyttää tietokehyksen viimeiset rivit

# Kysymys 17
?head
head(AgeWeight, 10) # esim. 10 ensimmäistä riviä

# Kysymys 18
dim(AgeWeight)	# 100, 6
ncol(AgeWeight)	# 6
nrow(AgeWeight)	# 100
# dim palauttaa ensin rivien määrän, sitten sarakkeiden

# Kysymys 19
AgeWeight$AGE
class(AgeWeight$AGE)
# ikä on numeerinen muuttuja (kokonaisluku eli integer)

# Kysymys 20
?tolower
tolower("CAR")						      # testi
ageweight <- AgeWeight					# kopioidaan tietokehys uuteen muuttujaan
names(ageweight) <- tolower(names(AgeWeight)) 	# muutetaan muuttujien nimet pieniksi kirjaimiksi
names(ageweight)		    				# tarkistetaan tulokset

# Kysymys 21
summary(AgeWeight)
# tietokehyksessä on kuusi muuttujaa
# kaksi muuttujaa (SMOKE1 ja SMOKE2) saavat arvoikseen merkkijonoja
# neljä muuta muuttujaa vaikuttaa olevan jatkuvia
# huom! katso summaryn antamia arvoja eri muuttujille: huomaatko mitään outoa? näitä käsitellään tarkemmin harjoitusmonisteessa R1.2

# -----

# 4. Aineiston poiminta

# Kysymys 22
m <- c(48,32,78,22,16,60)
# esim:
m[2:5]
m[c(2,3,4,5)]

# Kysymys 23
m[length(m)-1]

# Kysymys 24
AgeWeight[c(2,10,21),]

# Kysymys 25
AgeWeight[c(1,2,30:45),]

# Kysymys 26
AgeWeight[,c(3:4)]

# Kysymys 27
AgeWeight[54,3] # paino (WEIGHT) on 3. sarakkeessa

# Kysymys 28
AgeWeight[, c("WEIGHT", "AGE")]

# Kysymys 29
AgeWeight[c(91:100), c("MALE", "AGE")]

# Kysymys 30
100 + 35/(23*3) > 110 - 16/5.5
# FALSE eli ei ole

# Kysymys 31
which(AgeWeight$AGE > 60)		# listaa rivit, joiden havainnot täyttävät ehdon
AgeWeight[AgeWeight$AGE > 60,]	# poimitaan vain rivit, joiden havainnot täyttävät ehdon

# Kysymys 32
AgeWeight[10,6]		# rivin 10 sarake 6
AgeWeight[4:5,2:3]	# rivit 4:5, sarakkeet 2:3
AgeWeight[4:5,]		  # rivit 4:5, kaikki sarakkeet
AgeWeight[c(4,6),c(1,3)] # rivit 4 ja 6 ja sarakkeet 1 ja 3 
AgeWeight[-(4:5),1]	     # kaikki muut rivit paitsi 4 ja 5 sarakkeessa 1
	
# Kysymys 33
AgeWeight[10,"SMOKE2"]			# rivin 10 sarake 6
AgeWeight[4:5, c("AGE", "WEIGHT")]	# rivit 4:5, sarakkeet 2:3
AgeWeight[c(4,6), c("MALE", "WEIGHT")] # rivit 4 ja 6, sarakkeet 1 ja 3

# Kysymys 34
AgeWeight[AgeWeight$MALE == 1,]		# testataan, toimiiko poiminta
age_weight.male <- AgeWeight[AgeWeight$MALE == 1,]	# tallennetaan tulos uuteen tietokehykseen
summary(age_weight.male)	      	# tarkastellaan uutta tietokehystä


# -----

# Kysymys 35
x <- c(0,2,4,6,8,10)
y <- c(1,3,5,7,9,11)
x+y
# x ja y lasketaan yhteen alkiopari kerrallaan, tuloksena on samanpituinen vektori
x*y
# myös kertolasku toimii samoin

# Kysymys 36
AgeWeight$BMI <- (AgeWeight$WEIGHT)/((AgeWeight$HEIGHT)^2)
head(AgeWeight)

# Kysymys 37
sum(x)		          	# testaa yllä luotua vektoria x käyttäen
sum(AgeWeight$WEIGHT)	# tai datakehyksen muuttujaa käyttäen
# tulos on kaikkien vektorin / muuttujan alkioiden summa

# Kysymys 38
# monenlaisia c, rep, seq, matrix, head, tail, summary, names, class, dim, ...

# Kysymys 39
?sort
# järjestää vektorin alkiot nousevaan tai laskevaan järjestykseen

# Kysymys 40
# kirjastot ovat valmiiden funktioiden kokoelmia
# kirjastojen käyttö on kaksivaiheista:
# 1. asenna kirjasto koneellesi (vain kerran)
install.package('psych')
# 2. lataa kirjasto työskentelysessiosi käyttöön
library('psych')
