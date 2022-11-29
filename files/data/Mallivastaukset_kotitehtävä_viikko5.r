# PSY204
# Kotitehtävät, viikko 5
# Mallivastaukset
# Heini Saarimäki 7.10.2022

# -----

# 1. Aineiston valmistelu

# Asetetaan työskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/Kotitehtävät")

# Ladataan valmis aineisto:

suvilehto <- read.csv('https://hpsaarimaki.github.io/files/data/suvilehto_clean.csv', sep=";")
suvilehto$X <- NULL # poistan ylimääräisen sarakkeen

# Tarkastellaan aineistoa:
summary(suvilehto)  
dim(suvilehto) # kertoo tietokehyksen rivien ja sarakkeiden määrät
# Puuttuvia havaintoja on paljon (N=2236), mutta toisaalta N=9615 eli
# aineistoa jää silti yllin kyllin.

# Muutetaan faktorit faktoreiksi:
suvilehto$subid <- factor(suvilehto$subid)
suvilehto$sex <- factor(suvilehto$sex)
suvilehto$country <- factor(suvilehto$country)
suvilehto$person <- factor(suvilehto$person)

# Tarkastelen vielä muutoksia
summary(suvilehto) 
# Kiinnostavaa: sarake subid kertoo, että yhdeltä osallistujalta on 15 havaintoriviä
# eli käytännössä tässä aineistossa arviot jokaisen 15 henkilön (äiti, puoliso jne)
# tunnesiteestä, kosketuksen miellyttävyydestä ja eri kehonosien kosketettavuudesta.

# Kiinnitetään aineisto:
attach(suvilehto)

# ---

# 2. Aineiston tarkastelu

# Tallennetaan keski- ja hajontaluvut jatkuville muuttujille:
keskiluvut <- describe(suvilehto[c(3,6:10)])
write.csv(keskiluvut, 'suvilehto_keskiluvut.csv')

# Tallennetaan frekvenssit kategorisille muuttujille:
frekvenssit <- summary(suvilehto[c(2,4:5)])
write.csv(frekvenssit, 'suvilehto_frekvenssit.csv')

# Tarkastellaan, korreloivatko eri kehonosien kosketettavuudet keskenään:
library(psych)
corr.test(suvilehto[c(8:10)])
# Kaikki korrelaatiot ovat merkitseviä:
# Kasvot ja kädet: r=0.4, p<.001
# Kasvot ja jalat: r=0.67, p<.001
# Kädet ja jalat: r=0.35, p<.001

# Tarkastele laatikkokuvioiden avulla, eroaako eri kehonosien sallittavuusindeksi eri kulttuureissa. 
boxplot(face ~ country) # UK kasvoja saa koskea enemmän
boxplot(hand ~ country) # ei selkeitä kulttuurieroja
boxplot(leg ~ country)  # ei selkeitä kulttuurieroja tässä kuviossa, mutta
                        # jakauman vinous peittää efektiä alleen

# Tarkastellaan vielä histogrammeja, koska laatikkokuvioissa
# etenkin jalkojen osalta jakauma näytti vinolta:
par(mfrow=c(1,3)) 
hist(face)
hist(hand)
hist(leg)
# Jakaumat ovat todella vinoja, suurin osa osallistujista ei halua
# että kukaan koskee kasvoihin, käsiin tai jalkoihin.

# Datamuunnoksia voisi harkita, mutta niitä ei käytetty alkuperäisessä
# artikkelissa. Jatketaan siis alkuperäisten muuttujien kanssa
# huomioiden, että normaalijakaumaoletus ei täyty.

# ---

# 3. Regressioanalyysi

# Tutkimuskysymys:
# Vaikuttaako kulttuuritausta ja suhteen laatu kosketuksen sallittavuuteen eri kehonosissa?

# -

# Kasvot

# Testataan kahta eri mallia kasvoille:
lm1 <- lm(face~country+bond, data=suvilehto)
lm2 <- lm(face~country*bond, data=suvilehto)

# Oletukset:
plot(lm1)
# Lineaarisuusoletus ei vaikuta täyttyvän.
# Residuaalien normaalisuusoletus ei täyty.
# Residuaalien homoskedastistuus ei täyty, enemmän virheitä kun kosketettavuus kasvaa.
# Ei merkittäviä vierashavaintoja.
# Havaintojen riippumattomuus ei täyty, koska samalta osallistujalta
# useampi havainto.

plot(lm2)
# Samat ongelmat:
# Lineaarisuusoletus ei vaikuta täyttyvän.
# Residuaalien normaalisuusoletus ei täyty.
# Residuaalien homoskedastistuus ei täyty, enemmän virheitä kun kosketettavuus kasvaa.
# Ei merkittäviä vierashavaintoja.
# Havaintojen riippumattomuus ei täyty.

# Tarkastelu:
summary(lm1)
summary(lm2)

# Kumpi malli on parempi?
anova(lm1,lm2)
# Malli 2 sopii aineistoon paremmin, joten raportoidaan tulokset sen osalta:
# Kulttuurin ja tunnesiteen pää- ja yhdysvaikutukset sisältävä malli sopii 
# aineistoon ja selittää 15% kasvojen kosketettavuuden vaihtelusta 
# (F2, 7375)=424, p<.001, muokattu R^2=.15).
# Kulttuuri vaikuttaa siihen, miten tunneside ennustaa kasvojen kosketettavuutta (beta=0.006, p<.05).
# Kulttuuri vaikuttaa kasvojen kosketettavuuteen ylipäätään (beta=0.04, p<.01).
# Tunneside vaikuttaa kasvojen kosketettavuuteen ylipäätään (beta=0.04, p<.001).

# Kuvio:
library(ggplot2)
ggplot(suvilehto , aes(x=bond, y=face, color=as.factor(country) )) + 
  geom_point(size=1) +  
  facet_wrap(~country) +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE) +
  theme(legend.position="none")
# Kulttuurin ja tunnesiteen yhdysvaikutus ei vaikuta kuvion perusteella kovin voimakkaalta.
# Aineisto on niin iso, että pienikin ero tulee helposti merkitseväksi.
# Ei kovin kaunista. Ei ihme, että artikkelin kuvioissa käytettiin
# keskiarvoista dataa :)

# -

# Entä kädet?

# Luodaan mallit:
lm3 <- lm(hand~country+bond, data=suvilehto)
lm4 <- lm(hand~country*bond, data=suvilehto)

# Tarkastellaan niitä:
summary(lm3)
summary(lm4)

# Valitaan parempi malli:
anova(lm3,lm4)

# Malli 4 sopii aineistoon paremmin, joten raportoidaan tulokset sen osalta:
# Kulttuurin ja tunnesiteen pää- ja yhdysvaikutukset sisältävä malli sopii 
# aineistoon ja mutta selittää vain 4.8% käsien kosketettavuuden vaihtelusta 
# (F2, 7375)=124.3, p<.001, muokattu R^2=.048).
# Kulttuuri vaikuttaa siihen, miten tunneside ennustaa käsien kosketettavuutta (beta=-0.023, p<.001).
# Kulttuuri vaikuttaa käsien kosketettavuuteen ylipäätään (beta=0.14, p<.001).
# Tunneside vaikuttaa käsien kosketettavuuteen ylipäätään (beta=0.04, p<.001).

# Kuvio:
library(ggplot2)
ggplot(suvilehto , aes(x=bond, y=hand, color=as.factor(country) )) + 
  geom_point(size=1) +  
  facet_wrap(~country) +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE) +
  theme(legend.position="none")
# Japanissa tunnesiteen vaikutus on voimakkaampi.

# -

# Entä jalat:

# Luodaan mallit:
lm5 <- lm(leg~country+bond, data=suvilehto)
lm6 <- lm(leg~country*bond, data=suvilehto)

# Tarkastelu:
summary(lm5)
summary(lm6)

# Valitaan parempi malli:
anova(lm5,lm6)
# Malli 6 ei ole merkitsevästi parempi, joten malli 5
# sopii aineistoon paremmin yksinkertaisuusperiaatteen
# mukaan.
# Kulttuurin ja tunnesiteen yhdysvaikutus ei ollut merkitsevä.
# Kulttuurin päävaikutukset sisältävä malli sopii aineistoon 
# ja selittää 11.8% jalkojen kosketettavuuden vaihtelusta 
# (F2, 7376)=496, p<.001, muokattu R^2=.118).
# Kulttuuri vaikuttaa jalkojen kosketettavuuteen ylipäätään (beta=-0.03, p<.001).
# Tunneside vaikuttaa jalkojen kosketettavuuteen ylipäätään (beta=0.03, p<.001).

# Kuvio:
library(ggplot2)
ggplot(suvilehto , aes(x=bond, y=leg, color=as.factor(country) )) + 
  geom_point(size=1) +  
  facet_wrap(~country) +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE) +
  theme(legend.position="none")

# -

# Tulosten tarkastelu:

# Aineisto ei täytä useampia lineaarisen regression oletuksia.
# Aineisto ei ole normaalijakautunut: suurin osa osallistujista ei halunnut kenenkään koskettavan
# kasvoja, käsiä ja jalkoja.
# Otoskoko on suuri, joten lineaarisessa regressioanalyysissa pienetkin erot tulevat merkitseviksi.
# Tämä näkyy myös mallien melko pieninä varianssin selitysasteina, mutta selitysasteet eivät ole
# kovin luotettavia, koska regressioanalyysin oletukset eivät täyty. 
# Lisäksi riippumattomuusoletus ei täyty, joten analyysi tulisi toistaa esimerkiksi lineaarisia
# sekamalleja käyttäen. Tämä samalla pienentäisi otoskokoa, mikä vaikuttaisi erityisesti siihen, että
# pienet yhteydet eivät tulisi niin merkitseviksi.
# Tuloskuvien perusteella selkein kulttuuriero näkyi käsien kosketettavuuden ja tunnesiteen yhteydessä.
# Tuloksia ei ylipäätään voida pitää kovin luotettavina ja niitä olisi syytä vertailla aiempaan kirjallisuuteen
# tai toistaa uudella otoksella ja eri analyysimenetelmin, jotta pitäviä johtopäätöksiä voidaan tehdä.
# Tutkimus on eksploratiivinen, ja sen suurin hyöty on ilmiön kuvaaminen, joka mahdollista jatkotutkimusten
# suunnittelun.