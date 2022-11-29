# PSY204
# Kotiteht�v�t, viikko 5
# Mallivastaukset
# Heini Saarim�ki 7.10.2022

# -----

# 1. Aineiston valmistelu

# Asetetaan ty�skentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/Kotiteht�v�t")

# Ladataan valmis aineisto:

suvilehto <- read.csv('https://hpsaarimaki.github.io/files/data/suvilehto_clean.csv', sep=";")
suvilehto$X <- NULL # poistan ylim��r�isen sarakkeen

# Tarkastellaan aineistoa:
summary(suvilehto)  
dim(suvilehto) # kertoo tietokehyksen rivien ja sarakkeiden m��r�t
# Puuttuvia havaintoja on paljon (N=2236), mutta toisaalta N=9615 eli
# aineistoa j�� silti yllin kyllin.

# Muutetaan faktorit faktoreiksi:
suvilehto$subid <- factor(suvilehto$subid)
suvilehto$sex <- factor(suvilehto$sex)
suvilehto$country <- factor(suvilehto$country)
suvilehto$person <- factor(suvilehto$person)

# Tarkastelen viel� muutoksia
summary(suvilehto) 
# Kiinnostavaa: sarake subid kertoo, ett� yhdelt� osallistujalta on 15 havaintorivi�
# eli k�yt�nn�ss� t�ss� aineistossa arviot jokaisen 15 henkil�n (�iti, puoliso jne)
# tunnesiteest�, kosketuksen miellytt�vyydest� ja eri kehonosien kosketettavuudesta.

# Kiinnitet��n aineisto:
attach(suvilehto)

# ---

# 2. Aineiston tarkastelu

# Tallennetaan keski- ja hajontaluvut jatkuville muuttujille:
keskiluvut <- describe(suvilehto[c(3,6:10)])
write.csv(keskiluvut, 'suvilehto_keskiluvut.csv')

# Tallennetaan frekvenssit kategorisille muuttujille:
frekvenssit <- summary(suvilehto[c(2,4:5)])
write.csv(frekvenssit, 'suvilehto_frekvenssit.csv')

# Tarkastellaan, korreloivatko eri kehonosien kosketettavuudet kesken��n:
library(psych)
corr.test(suvilehto[c(8:10)])
# Kaikki korrelaatiot ovat merkitsevi�:
# Kasvot ja k�det: r=0.4, p<.001
# Kasvot ja jalat: r=0.67, p<.001
# K�det ja jalat: r=0.35, p<.001

# Tarkastele laatikkokuvioiden avulla, eroaako eri kehonosien sallittavuusindeksi eri kulttuureissa. 
boxplot(face ~ country) # UK kasvoja saa koskea enemm�n
boxplot(hand ~ country) # ei selkeit� kulttuurieroja
boxplot(leg ~ country)  # ei selkeit� kulttuurieroja t�ss� kuviossa, mutta
                        # jakauman vinous peitt�� efekti� alleen

# Tarkastellaan viel� histogrammeja, koska laatikkokuvioissa
# etenkin jalkojen osalta jakauma n�ytti vinolta:
par(mfrow=c(1,3)) 
hist(face)
hist(hand)
hist(leg)
# Jakaumat ovat todella vinoja, suurin osa osallistujista ei halua
# ett� kukaan koskee kasvoihin, k�siin tai jalkoihin.

# Datamuunnoksia voisi harkita, mutta niit� ei k�ytetty alkuper�isess�
# artikkelissa. Jatketaan siis alkuper�isten muuttujien kanssa
# huomioiden, ett� normaalijakaumaoletus ei t�yty.

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
# Lineaarisuusoletus ei vaikuta t�yttyv�n.
# Residuaalien normaalisuusoletus ei t�yty.
# Residuaalien homoskedastistuus ei t�yty, enemm�n virheit� kun kosketettavuus kasvaa.
# Ei merkitt�vi� vierashavaintoja.
# Havaintojen riippumattomuus ei t�yty, koska samalta osallistujalta
# useampi havainto.

plot(lm2)
# Samat ongelmat:
# Lineaarisuusoletus ei vaikuta t�yttyv�n.
# Residuaalien normaalisuusoletus ei t�yty.
# Residuaalien homoskedastistuus ei t�yty, enemm�n virheit� kun kosketettavuus kasvaa.
# Ei merkitt�vi� vierashavaintoja.
# Havaintojen riippumattomuus ei t�yty.

# Tarkastelu:
summary(lm1)
summary(lm2)

# Kumpi malli on parempi?
anova(lm1,lm2)
# Malli 2 sopii aineistoon paremmin, joten raportoidaan tulokset sen osalta:
# Kulttuurin ja tunnesiteen p��- ja yhdysvaikutukset sis�lt�v� malli sopii 
# aineistoon ja selitt�� 15% kasvojen kosketettavuuden vaihtelusta 
# (F2, 7375)=424, p<.001, muokattu R^2=.15).
# Kulttuuri vaikuttaa siihen, miten tunneside ennustaa kasvojen kosketettavuutta (beta=0.006, p<.05).
# Kulttuuri vaikuttaa kasvojen kosketettavuuteen ylip��t��n (beta=0.04, p<.01).
# Tunneside vaikuttaa kasvojen kosketettavuuteen ylip��t��n (beta=0.04, p<.001).

# Kuvio:
library(ggplot2)
ggplot(suvilehto , aes(x=bond, y=face, color=as.factor(country) )) + 
  geom_point(size=1) +  
  facet_wrap(~country) +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE) +
  theme(legend.position="none")
# Kulttuurin ja tunnesiteen yhdysvaikutus ei vaikuta kuvion perusteella kovin voimakkaalta.
# Aineisto on niin iso, ett� pienikin ero tulee helposti merkitsev�ksi.
# Ei kovin kaunista. Ei ihme, ett� artikkelin kuvioissa k�ytettiin
# keskiarvoista dataa :)

# -

# Ent� k�det?

# Luodaan mallit:
lm3 <- lm(hand~country+bond, data=suvilehto)
lm4 <- lm(hand~country*bond, data=suvilehto)

# Tarkastellaan niit�:
summary(lm3)
summary(lm4)

# Valitaan parempi malli:
anova(lm3,lm4)

# Malli 4 sopii aineistoon paremmin, joten raportoidaan tulokset sen osalta:
# Kulttuurin ja tunnesiteen p��- ja yhdysvaikutukset sis�lt�v� malli sopii 
# aineistoon ja mutta selitt�� vain 4.8% k�sien kosketettavuuden vaihtelusta 
# (F2, 7375)=124.3, p<.001, muokattu R^2=.048).
# Kulttuuri vaikuttaa siihen, miten tunneside ennustaa k�sien kosketettavuutta (beta=-0.023, p<.001).
# Kulttuuri vaikuttaa k�sien kosketettavuuteen ylip��t��n (beta=0.14, p<.001).
# Tunneside vaikuttaa k�sien kosketettavuuteen ylip��t��n (beta=0.04, p<.001).

# Kuvio:
library(ggplot2)
ggplot(suvilehto , aes(x=bond, y=hand, color=as.factor(country) )) + 
  geom_point(size=1) +  
  facet_wrap(~country) +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE) +
  theme(legend.position="none")
# Japanissa tunnesiteen vaikutus on voimakkaampi.

# -

# Ent� jalat:

# Luodaan mallit:
lm5 <- lm(leg~country+bond, data=suvilehto)
lm6 <- lm(leg~country*bond, data=suvilehto)

# Tarkastelu:
summary(lm5)
summary(lm6)

# Valitaan parempi malli:
anova(lm5,lm6)
# Malli 6 ei ole merkitsev�sti parempi, joten malli 5
# sopii aineistoon paremmin yksinkertaisuusperiaatteen
# mukaan.
# Kulttuurin ja tunnesiteen yhdysvaikutus ei ollut merkitsev�.
# Kulttuurin p��vaikutukset sis�lt�v� malli sopii aineistoon 
# ja selitt�� 11.8% jalkojen kosketettavuuden vaihtelusta 
# (F2, 7376)=496, p<.001, muokattu R^2=.118).
# Kulttuuri vaikuttaa jalkojen kosketettavuuteen ylip��t��n (beta=-0.03, p<.001).
# Tunneside vaikuttaa jalkojen kosketettavuuteen ylip��t��n (beta=0.03, p<.001).

# Kuvio:
library(ggplot2)
ggplot(suvilehto , aes(x=bond, y=leg, color=as.factor(country) )) + 
  geom_point(size=1) +  
  facet_wrap(~country) +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE) +
  theme(legend.position="none")

# -

# Tulosten tarkastelu:

# Aineisto ei t�yt� useampia lineaarisen regression oletuksia.
# Aineisto ei ole normaalijakautunut: suurin osa osallistujista ei halunnut kenenk��n koskettavan
# kasvoja, k�si� ja jalkoja.
# Otoskoko on suuri, joten lineaarisessa regressioanalyysissa pienetkin erot tulevat merkitseviksi.
# T�m� n�kyy my�s mallien melko pienin� varianssin selitysasteina, mutta selitysasteet eiv�t ole
# kovin luotettavia, koska regressioanalyysin oletukset eiv�t t�yty. 
# Lis�ksi riippumattomuusoletus ei t�yty, joten analyysi tulisi toistaa esimerkiksi lineaarisia
# sekamalleja k�ytt�en. T�m� samalla pienent�isi otoskokoa, mik� vaikuttaisi erityisesti siihen, ett�
# pienet yhteydet eiv�t tulisi niin merkitseviksi.
# Tuloskuvien perusteella selkein kulttuuriero n�kyi k�sien kosketettavuuden ja tunnesiteen yhteydess�.
# Tuloksia ei ylip��t��n voida pit�� kovin luotettavina ja niit� olisi syyt� vertailla aiempaan kirjallisuuteen
# tai toistaa uudella otoksella ja eri analyysimenetelmin, jotta pit�vi� johtop��t�ksi� voidaan tehd�.
# Tutkimus on eksploratiivinen, ja sen suurin hy�ty on ilmi�n kuvaaminen, joka mahdollista jatkotutkimusten
# suunnittelun.