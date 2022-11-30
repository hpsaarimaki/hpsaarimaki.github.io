# PSY204 syksy 2022
# Kotiteht‰v‰ viikko 7: Faktorianalyysi
# Mallivastaukset
# Heini Saarim‰ki 24.10.2022
# 
# ----

# Asetetaan tyˆskentelykansio
setwd("C:/Users/sbhesa/Documents/Opetus/")

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto:

dubois <- read.csv("2022-2023/PSY204 - syksy 2022/Kotiteht‰v‰t/hcp_clean.csv", sep=";")

# Tarkistetaan puuttuvat havainnot:
library(psych)
describe(dubois)
anyNA(dubois) 

# ei puuttuvia havaintoja, N = 884

# Muutetaan faktorit faktoreiksi:
dubois$Subject <- factor(dubois$Subject)
dubois$Gender <- factor(dubois$Gender)

# Kiinnitet‰‰n tietokehys:
attach(dubois)

# -

# 2. Aineiston tarkastelu

# Tallennetaan teht‰muuttujien keski- ja hajontaluvut:
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
# Kuvion voi tallentaa k‰sin esim. kuvavalikosta Export --> Save as image

# Vaihtoehtoisesti voidaan tallentaa korrelaatiomatriisi taulukkona:
korrelaatiot <- corr.test(dubois[4:13]) # tallennetaan ensin korrelaatiomatriisi
write.csv(korrelaatiot$r, 'dubois_korrelaatiot_r.csv') # tallennetaan korrelaatiokertoimet tiedostoksi
write.csv(korrelaatiot$p, 'dubois_korrelaatiot_p.csv') # tallennetaan p-arvot tiedostoksi

# Korrelaatioiden vaihteluv‰li:
range(korrelaatiot$r)

# Aineiston faktoroitumisen arviointi:

# Otetaan MSA-arvot ja tallennetaan ne olioon nimelt‰ 'msa':
msa <- KMO(dubois[4:13])
msa

# MSA-arvot lˆytyv‰t oliosta kohdasta MSAi, joten poimitaan ne ja tallennetaan taulukkoon:
write.csv(msa$MSAi, 'msa.csv')

# -

# 3. Faktorimallin suunnittelu ja toteutus

# Faktorien m‰‰r‰n valinta
# 1) Scree-kuvio
scree(dubois[4:13])
# 2) Parallel analysis
fa.parallel(dubois[4:13])
# Kummankin perusteella 4 n‰ytt‰‰ toimivalta

# Ekstraktointimenetelm‰ksi ML
# Rotatointimenetelm‰ksi vinokulmarotaatio
# Perustelut: niit‰ k‰ytettiin alkuper‰isess‰ 
# artikkelissa ja haluan toistaa analyysit.

fa4 <- fa(dubois[4:13],
          nfactor=4,
          fm="ml",
          max.iter=100,
          rotate="oblimin"
          )

fa.diagram(fa4)
# IWRD_TOT ei lataa millek‰‰n faktorille yli .3 latauksilla,
# harkitsen poistamista. Katson kuitenkin
# tarkemmat tulokset ensin t‰lle mallille.

# ---

# 4. Mallin tulkinta

# Yleistulokset:
fa4
# Faktorit korreloivat kesken‰‰n, eli 
# vinokulmarotaatio oli hyv‰ valinta.

# 1) Faktoriratkaisun sopivuus:
# Khiin neliˆtesti: khii2(11) = 13.96, p<.24 eli ei-merkitsev‰. Malli sopii hyvin.
# TLI = 0.99 eli malli sopii hyvin.
# RMSEA = 0.017 eli malli sopii hyvin.

# 2) Faktorilataukset:
library(plot.matrix)
plot(loadings(fa4), cex=0.5, axis.row = list(side=2, las=1))
# IWRD_TOT lataa ML3:lle 0.22, kuten alkuper‰isartikkelissa.

# 3) Muuttujien kommunaliteetit:
fa4$communality
# Kommunaliteetit <.3: 
# ListSort 0.28, IWRD_TOT 0.098
# ListSort viel‰ kohtuullisen rajoissa, mutta
# IWRD_TOT sopii malliin huonosti.

# 4) Faktoreiden selitt‰m‰ varianssi:
# Faktorimallin selitysosuus: 45%
# Yksitt‰isten faktorien selitysosuus:
# ML1 14%, ML2 14%, ML4 9%, ML3 8%

# 5) Faktoreiden nime‰minen
# ML1: kristalloitunut ‰lykkyys
# ML2: prosessointinopeus
# ML3: muisti
# ML4: visuospatiaaliset kyvyt

# -

# Faktoriratkaisun tavoitteiden arviointi:

# A) Faktorit selitt‰v‰t yhteisvaihtelusta 45%, mik‰ on varsin suuri osuus.

# B) Faktoreita on mahdollisimman v‰h‰n: vyˆrykuvion ja rinnakkaisanalyysin tulokset
# olivat varsin selke‰t ja tukivat 4 faktorin ratkaisua.

# C) Malliin tulee mahdollisimman paljon pieni‰ ja suuria latauksia, keskinkertaisia v‰h‰n:
# faktorit n‰ytt‰v‰t erottuvan selke‰sti latausmatriisin perusteella.

# D) Faktoreille on mielek‰s tulkinta: kognitiivisen kyvykkyyden teorioiden perusteella
# faktorit oli helppo tulkita.

# Kokonaisuudessaan malli n‰ytt‰‰ hyv‰lt‰, mutta IWRD_TOT ei sovi malliin.
# Eri m‰‰r‰ faktoreita ei vaikuta hyv‰lt‰ ratkaisulta, koska rinnakkaisanalyysin perusteella
# faktoreiden optimaalinen m‰‰r‰ oli niin selke‰.
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
# Mallin sopivuus entist‰kin parempi
# Muuttujien kommunaliteetit paranevat: nyt kaikilla >.3
# Mallin selitysaste paranee (49%)
library(plot.matrix)
plot(loadings(fa4.new), cex=0.5, axis.row = list(side=2, las=1))

# N‰iden tulosten perusteella harkitsisin muuttujan IWRD_TOT poistamista lopullisesti faktoriratkaisusta.
#

# ---

# 5. Faktorianalyysin raportointi

# Kaikki teht‰v‰muuttujat korreloivat kesken‰‰n. Rinnakkaisanalyysin perusteella 
# teht‰v‰muuttujat muodostivat nelj‰faktoria, joten teht‰v‰muuttujien taustalla 
# olevia latentteja faktoreita tutkittiin ekploratiivisella faktoriratkaisulla, jossa
# faktoreiden m‰‰r‰ksi valittiin nelj‰. Ekstraktointimenetelm‰n‰ k‰ytettiin p‰‰akselifaktorointia.
# Rotatointimenetelm‰ksi valittiin vinokulmarotaatio, koska kognition eri osa-alueiden 
# oletettiin korreloivan kesken‰‰n.

# Nelj‰n faktorin ratkaisu sopi malliin hyvin (khii2(11) = 13.96, p<.24; TLI = 0.99; RMSEA = 0.017).
# Nelj‰ faktoria voitiin tulkita 1) kristalloituneeksi kyvykkyydeksi (Vocab, ReadEng), 
# 2) prosessointinopeudeksi (CardSort, Flanker, ProcSpeed), 3) visuospatiaaliseksi kyvykkyydeksi (VSPLOT, PMAT)
# ja 4) muistiksi (PicSeq, ListSort). Yksitt‰isist‰ teht‰v‰muuttujista vain IWRD_TOT latautui faktoreille heikosti (kommunaliteetti = 0.098, kun
# hyv‰ksytt‰v‰n kommunaliteetin rajana voidaan pit‰‰ .3), joten faktorianalyysi ajettiin viel‰ ilman t‰t‰
# muuttujaa. Muuttujan j‰tt‰minen pois faktoriratkaisusta paransi mallin sopivuutta entisest‰‰n
# (khii2(6) = 4.18, p<.65; TLI = 1.01; RMSEA = 0).

# Nelj‰n faktorin ratkaisu ilman IWRD_TOT-muuttujaa selitti yhteens‰ 49% teht‰viss‰ suoriutumisen
# vaihtelusta (yksitt‰isten faktoreiden selitysosuudet 8-16%).


