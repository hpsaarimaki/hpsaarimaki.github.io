# PSY204 syksy 2022
# Kotitehtävä viikko 7: Faktorianalyysi
# Mallivastaukset
# Heini Saarimäki 24.10.2022
# 
# ----

# Asetetaan työskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:

dubois <- read.csv("2022-2023/PSY204 - syksy 2022/Kotitehtävät/hcp_clean.csv", sep=";")

# Tarkistetaan puuttuvat havainnot:
library(psych)
describe(dubois)
anyNA(dubois) 

# ei puuttuvia havaintoja, N = 884

# Muutetaan faktorit faktoreiksi:
dubois$Subject <- factor(dubois$Subject)
dubois$Gender <- factor(dubois$Gender)

# Kiinnitetään tietokehys:
attach(dubois)

# -

# 2. Aineiston tarkastelu

# Tallennetaan tehtämuuttujien keski- ja hajontaluvut:
keskiluvut <- describe(dubois[4:13])
write.csv(keskiluvut, 'dubois_keskiluvut.csv')

# Tarkastellaan muuttujien jakaumia:
par(mfrow=c(2,5))
hist(dubois$CardSort_Unadj, main="CardSort")
hist(dubois$Flanker_Unadj, main="Flanker")
hist(dubois$ListSort_Unadj, main="ListSort")
hist(dubois$PicSeq_Unadj, main="PicSeq")
hist(dubois$PicVocab_Unadj, main="PicVocab")
hist(dubois$ProcSpeed_Unadj, main="ProcSpeed")
hist(dubois$ReadEng_Unadj, main="ReadEng")
hist(dubois$IWRD_TOT, main="IWRD")
hist(dubois$PMAT24_A_CR, main="PMAT")
hist(dubois$VSPLOT_TC, main="VSPLOT")

# Tarkastellaan korrelaatiomatriisia visuaalisesti:
library(ggplot2)
library(GGally)
ggcorr(dubois[4:13])
# Kuvion voi tallentaa käsin esim. kuvavalikosta Export --> Save as image

# Vaihtoehtoisesti voidaan tallentaa korrelaatiomatriisi taulukkona:
korrelaatiot <- corr.test(dubois[4:13]) # tallennetaan ensin korrelaatiomatriisi
write.csv(korrelaatiot$r, 'dubois_korrelaatiot_r.csv') # tallennetaan korrelaatiokertoimet tiedostoksi
write.csv(korrelaatiot$p, 'dubois_korrelaatiot_p.csv') # tallennetaan p-arvot tiedostoksi

# Korrelaatioiden vaihteluväli:
range(korrelaatiot$r)

# Aineiston faktoroitumisen arviointi:

# Otetaan MSA-arvot ja tallennetaan ne olioon nimeltä 'msa':
msa <- KMO(dubois[4:13])
msa

# MSA-arvot löytyvät oliosta kohdasta MSAi, joten poimitaan ne ja tallennetaan taulukkoon:
write.csv(msa$MSAi, 'msa.csv')

# -

# 3. Faktorimallin suunnittelu ja toteutus

# Faktorien määrän valinta
# 1) Scree-kuvio
scree(dubois[4:13])
# 2) Parallel analysis
fa.parallel(dubois[4:13])
# Kummankin perusteella 4 näyttää toimivalta

# Ekstraktointimenetelmäksi ML
# Rotatointimenetelmäksi vinokulmarotaatio
# Perustelut: niitä käytettiin alkuperäisessä 
# artikkelissa ja haluan toistaa analyysit.

fa4 <- fa(dubois[4:13],
          nfactor=4,
          fm="ml",
          max.iter=100,
          rotate="oblimin"
          )

fa.diagram(fa4)
# IWRD_TOT ei lataa millekään faktorille yli .3 latauksilla,
# harkitsen poistamista. Katson kuitenkin
# tarkemmat tulokset ensin tälle mallille.

# ---

# 4. Mallin tulkinta

# Yleistulokset:
fa4
# Faktorit korreloivat keskenään, eli 
# vinokulmarotaatio oli hyvä valinta.

# 1) Faktoriratkaisun sopivuus:
# Khiin neliötesti: khii2(11) = 13.96, p<.24 eli ei-merkitsevä. Malli sopii hyvin.
# TLI = 0.99 eli malli sopii hyvin.
# RMSEA = 0.017 eli malli sopii hyvin.

# 2) Faktorilataukset:
library(plot.matrix)
plot(loadings(fa4), cex=0.5, axis.row = list(side=2, las=1))
# IWRD_TOT lataa ML3:lle 0.22, kuten alkuperäisartikkelissa.

# 3) Muuttujien kommunaliteetit:
fa4$communality
# Kommunaliteetit <.3: 
# ListSort 0.28, IWRD_TOT 0.098
# ListSort vielä kohtuullisen rajoissa, mutta
# IWRD_TOT sopii malliin huonosti.

# 4) Faktoreiden selittämä varianssi:
# Faktorimallin selitysosuus: 45%
# Yksittäisten faktorien selitysosuus:
# ML1 14%, ML2 14%, ML4 9%, ML3 8%

# 5) Faktoreiden nimeäminen
# ML1: kristalloitunut älykkyys
# ML2: prosessointinopeus
# ML3: muisti
# ML4: visuospatiaaliset kyvyt

# -

# Faktoriratkaisun tavoitteiden arviointi:

# A) Faktorit selittävät yhteisvaihtelusta 45%, mikä on varsin suuri osuus.

# B) Faktoreita on mahdollisimman vähän: vyörykuvion ja rinnakkaisanalyysin tulokset
# olivat varsin selkeät ja tukivat 4 faktorin ratkaisua.

# C) Malliin tulee mahdollisimman paljon pieniä ja suuria latauksia, keskinkertaisia vähän:
# faktorit näyttävät erottuvan selkeästi latausmatriisin perusteella.

# D) Faktoreille on mielekäs tulkinta: kognitiivisen kyvykkyyden teorioiden perusteella
# faktorit oli helppo tulkita.

# Kokonaisuudessaan malli näyttää hyvältä, mutta IWRD_TOT ei sovi malliin.
# Eri määrä faktoreita ei vaikuta hyvältä ratkaisulta, koska rinnakkaisanalyysin perusteella
# faktoreiden optimaalinen määrä oli niin selkeä.
# Kokeillaan poistaa IWRD_TOT kokonaan:
fa4.new <- fa(dubois[c(4:10,12:13)],
          nfactor=4,
          fm="ml",
          max.iter=100,
          rotate="oblimin"
)

# Tulokset:
fa.diagram(fa4.new)
fa4.new
# Mallin sopivuus entistäkin parempi
# Muuttujien kommunaliteetit paranevat: nyt kaikilla >.3
# Mallin selitysaste paranee (49%)
library(plot.matrix)
plot(loadings(fa4.new), cex=0.5, axis.row = list(side=2, las=1))

# Näiden tulosten perusteella harkitsisin muuttujan IWRD_TOT poistamista lopullisesti faktoriratkaisusta.
#

# ---

# 5. Faktorianalyysin raportointi

# Kaikki tehtävämuuttujat korreloivat keskenään. Rinnakkaisanalyysin perusteella 
# tehtävämuuttujat muodostivat neljäfaktoria, joten tehtävämuuttujien taustalla 
# olevia latentteja faktoreita tutkittiin ekploratiivisella faktoriratkaisulla, jossa
# faktoreiden määräksi valittiin neljä. Ekstraktointimenetelmänä käytettiin pääakselifaktorointia.
# Rotatointimenetelmäksi valittiin vinokulmarotaatio, koska kognition eri osa-alueiden 
# oletettiin korreloivan keskenään.

# Neljän faktorin ratkaisu sopi malliin hyvin (khii2(11) = 13.96, p<.24; TLI = 0.99; RMSEA = 0.017).
# Neljä faktoria voitiin tulkita 1) kristalloituneeksi kyvykkyydeksi (Vocab, ReadEng), 
# 2) prosessointinopeudeksi (CardSort, Flanker, ProcSpeed), 3) visuospatiaaliseksi kyvykkyydeksi (VSPLOT, PMAT)
# ja 4) muistiksi (PicSeq, ListSort). Yksittäisistä tehtävämuuttujista vain IWRD_TOT latautui faktoreille heikosti (kommunaliteetti = 0.098, kun
# hyväksyttävän kommunaliteetin rajana voidaan pitää .3), joten faktorianalyysi ajettiin vielä ilman tätä
# muuttujaa. Muuttujan jättäminen pois faktoriratkaisusta paransi mallin sopivuutta entisestään
# (khii2(6) = 4.18, p<.65; TLI = 1.01; RMSEA = 0).

# Neljän faktorin ratkaisu ilman IWRD_TOT-muuttujaa selitti yhteensä 49% tehtävissä suoriutumisen
# vaihtelusta (yksittäisten faktoreiden selitysosuudet 8-16%).


