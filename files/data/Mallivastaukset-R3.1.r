# PSY204
# Harjoitusmoniste R3.1
# Heini Saarimäki 20.9.2022

# -----

# 1. Analyysin jatkaminen

# Asetetaan ensin työskentelykansio:
setwd("C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/R-harkat")

  
# Jos haluat käyttää alla olevaa source-komentoa, 
# varmista, että viime viikon skripti on työskentelykansiossasi.
  
# Ajetaan viime viikon analyysit.
# Voit käyttää yläreunan "source"-nappulaa, jos avaat viime viikon skriptin editoriin...
# ... tai ajaa viime viikon skriptin suoraan komentoriviltä:
source("harkat-R2.2.r")

# Tarkista, että tietokehys data ja mallit A1, A2 ja A3 näkyvät oikean reunan muuttujaluettelossa.

# ---

# 2. Oletusten tarkastelu

# Kysymys 1: Vierashavainnot

# Ladataan tarvittava kirjasto, asenna tarvittaessa:
library(rstatix)

# Koko aineisto
data %>%
  identify_outliers(EPAMIELLYTTAVYYS)
# ei vierashavaintoja

# Koetilanteittan harjoitusryhmän mukaan:
vierashavainnot <- data %>%
  group_by(RYHMA, HARJOITUS, VETO) %>%
  identify_outliers("EPAMIELLYTTAVYYS")
print(vierashavainnot, n=23)

# Vierashavainnot eivät ole merkitsevästi poikkeavia.

# Kysymys 2: normaalijakautuneisuus

# Ladataan tarvittava kirjasto, asenna tarvittaessa:
library(performance)

# Malli A1:
A1_is_norm <- check_normality(A1)
A1_is_norm
plot(A1_is_norm)
plot(A1_is_norm, type="qq")
plot(A1_is_norm, type="qq", detrend=TRUE)
# residuaalit normaalisti jakautuneet

# Malli A2:
A2_is_norm <- check_normality(A2)
A2_is_norm
plot(A2_is_norm, type="qq")
# residuaalit ei normaalisti jakautuneet

# Malli A3:
A3_is_norm <- check_normality(A3)
A3_is_norm
plot(A3_is_norm, type="qq")
# residuaalit ovat normaalisti jakautuneet

# Kysymys 3: varianssien yhtäsuuruus
check_sphericity(A1)
check_sphericity(A2)
check_sphericity(A3)
# varianssit eivät ole yhtä suuria missään mallissa

# Kysymys 14:
# Varianssien yhtäsuuruusoletuksen saa korjattua sfäärisyyskorjauksilla
# Mutta ryhmäkoot ovat varsin pieniä, vain 13 osallistujaa per ryhmä
# Lisäksi normaalijakautuneisuusoletus on ongelma
# Toistettujen mittausten varianssianalyysin sijaan kannattaisi harkita monitasoregressiomalleja

# ---

# 3. Tilastollisen testin tulosten tarkastelu

# Malli A1: Vedon päävaikutus

# Kysymys 5:

# Tutkitaan tunnuslukuja:
summary(A1)
anova(A1, es="pes")
# Koettu epämiellyttävyys muuttuu tilastollisesti merkitsevästi yhden harjoituskerran aikana (F(3,75)=59.39, p[GG]<.001, pes=.70).
# Tulosten tarkasteluun käytettiin Greenhouse-Geisserin sfäärisyyskorjausta, koska varianssit eivät olleet joka solussa yhtä suuret.

# Kysymys 6:
library(emmeans)

# Tallennetaan referenssitaulukko:
ref1 <- emmeans(A1, specs="VETO")

# Määritetään kontrasti:
kontrasti_1 <- c(1,-0.33,-0.33,-0.33)

# Testataan kontrastia:
summary(contrast(ref1, list(first_vs_others = kontrasti_1), adjust="holm"))

# Ensimmäinen veto eroaa kaikista muista (t(25)=7.48, Bonferroni-Holm-korjattu p<.0001)

# Kysymys 7:

# Yksinkertainen laatikkokuvio:
boxplot(EPAMIELLYTTAVYYS~VETO)

# Näytetään tulokset tuunattuna laatikkokuviona:
library(ggplot2) # ladataan kirjasto, asenna tarvittaessa
ggplot(data, aes(x=VETO, y=EPAMIELLYTTAVYYS)) +
  geom_boxplot() +
  labs(y = "Epämiellyttävyys", x = "Vedon järjestysnumero") + 
  theme(legend.title=element_blank())

# ---

# Malli A2: Vedon ja harjoituksen pää- ja yhdysvaikutukset

# Tutkitaan tunnuslukuja:
summary(A2)
anova(A2, es="pes")

# Harjoituksen aikaisen epämiellyttävyyden kokeminen ei muutu harjoittelun myötä (F(15,375)=1.36, p[GG]=.23).
# Harjoituksen aikana epämiellyttävyys muuttuu (F(3,75)=59.4, p[GG]<.001, pes=.704).
# Epämiellyttävyys muuttuu myös harjoituskertojen myötä (F(5,125)=6.4, p[GG]<.01, pes=.205).
# Muutos harjoituksen aikana on suurempi kuin harjoituskertojen myötä tapahtuva muutos (ks. efektikoot).

# Kysymys 9:

A2_posthoc1 <- emmeans(A2, specs=pairwise ~ VETO, adjust = "holm")
A2_posthoc2 <- emmeans(A2, specs=pairwise ~ HARJOITUS, adjust = "holm")

A2_posthoc1$contrasts

# Kaikkien vetoparien erot ovat tilastollisesti merkitseviä.
# Nämä voisi ehkä näyttää taulukkona, koska lukuja on niin paljon.
# Tallennetaan siis taulukko:
write.csv(A2_posthoc1$contrasts, "A2_veto_posthoc.csv")

A2_posthoc2$contrasts
# Bonferroni-Holm-korjauksen jälkeen merkitsevä ero löytyy vain 
# ensimmäisen ja viimeisen harjoituksen väliltä (t(25)=3.41, p<.05).

# Kysymys 10:
A2_bonf1 <- emmeans(A2, specs=pairwise ~ VETO, adjust = "bonferroni")
A2_bonf2 <- emmeans(A2, specs=pairwise ~ HARJOITUS, adjust = "bonferroni")

A2_bonf1$contrasts
# Konservatiivisemman Bonferroni-korjauksen käyttö ei muuta tuloksia:
# kaikki parittaiset vertailut edelleen merkitseviä.

A2_bonf2$contrasts
# Myös näiden post hoc-vertailuiden tulokset ovat edelleen samat.

# Kysymys 11:

# Näytetään tulokset interaktiokuviona:
interaction.plot(VETO, HARJOITUS, EPAMIELLYTTAVYYS,
                 type="b", col=c(1:6),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="VETO",
                 ylab="EPAMIELLYTTAVYYS",
                 main="Interaktiokuvio")
# Kuvio näyttää eri harjoituskertojen aikaisen koetun epämiellyttävyyden muutoksen.
# Epämiellyttävyys kasvaa aina harjoituskerran aikana.
# Ensimmäisessä harjoituksessa koettu epämiellyttävyys on suurin.
# Koettu epämiellyttävyys laskee harjoittelun myötä.

# ---

# Malli A3:

# Kysymys 12:

summary(A3)
anova(A3, es="pes")

# Ryhmät eroavat toisistaan vain harjoituskerran sisäisen muutoksen osalta (ryhmän ja vedon yhdysvaikutus, F(3, 72)=8.6, p[GG]<.01, pes=0.26).
# Epämiellyttävyyden muutos harjoituskerran sisällä riippuu siis ryhmästä.
# Vedon päävaikutus on merkitsevä, eli harjoituskerran sisällä tapahtuu myös keskimäärin muutosta (F(3,72)=77.4, p[GG]<.001, pes=.76).

# Kysymys 13:

# Post hoc -testaus vedon vaikutukselle tehtiin jo mallin A2 kohdalla, joten
# tässä tarkastellaan vain yhdysvaikutusta:

A3_posthoc <- emmeans(A3, specs=pairwise ~ RYHMA:VETO, adjust="holm")

# Parittaisten t-testien tulokset
A3_posthoc$contrasts

# Vedon merkitys eri ryhmissä: 
# Epämiellyttävyyden muutos näkyy HIIT-ryhmässä saman tien, ja tasaantuu kolmannen vedon jälkeen.
# Epämiellyttävyyden muutos näkyy MICT-ryhmässä vasta toisen vedon jälkeen.

# Kysymys 14:

# Näytetään tulokset interaktiokuviona:
interaction.plot(VETO, RYHMA, EPAMIELLYTTAVYYS,
                 type="b", col=c(1:6),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="VETO",
                 ylab="EPAMIELLYTTAVYYS",
                 main="Interaktiokuvio")
# Harjoituskerran aikana koettu epämiellyttävyys kasvaa jyrkemmin HIIT-ryhmässä.
# Tätä myös post hoc -vertailut osoittivat.


