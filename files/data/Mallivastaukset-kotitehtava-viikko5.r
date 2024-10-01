# PSY.204 syksy 2024
# Mallivastaukset
# Kotitehtävät viikko 5
# Heini Saarimäki

# ---

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Kotitehtävät/')

# Lataa aineisto, aseta desimaalimerkiksi pilkku
library(openxlsx)
robo <- read.xlsx('Linnunsalo2023.xlsx')

# ---

# 1. Aineiston valmistelu

# Muutetaan aineisto leveästä pitkään muotoon:
library(tidyr)
robo_long <- robo %>%
  pivot_longer(
    cols = c("N170-left.DIR", "N170-left.AVE", "N170-right.DIR", "N170-right.AVE", 
             "P300-left.DIR", "P300-left.AVE", "P300-right.DIR", "P300-right.AVE"),
    names_to = c(".value", "Hemisphere", "Gaze"),  # Luo uudet sarakkeet
    names_pattern = "(.*)-(.*)\\.(.*)"             # Uusien sarakkeiden nimien erottelu
  )

# Tarkista muuttujien tietotyypit
summary(robo_long)

# Kategoriset muuttujat faktoreiksi
robo_long$ID <- factor(robo_long$ID)
robo_long$Group <- factor(robo_long$Group)
robo_long$Hemisphere <- factor(robo_long$Hemisphere)
robo_long$Gaze <- factor(robo_long$Gaze)

# Tarkista muutokset
summary(robo_long)

# Poista huonolaatuiset osallistujat 15, 20 ja 59
# Tarkista rivit, joilla nämä osallistujat sijaitsevat:
which(robo_long$ID %in% c(15, 20, 59))
# Poista nämä rivit:
robo_final <- robo_long[-(which(robo_long$ID %in% c(15, 20, 59))),]

# Tarkista muutokset
summary(robo_final)

# Poikkeavien havaintojen tarkastelu:
boxplot(robo_final$N170)
boxplot(robo_final$P300)

# Kiinnitä tietokehys
attach(robo_final)

# ---

# 2. Aineiston tarkastelu

# Tarkista, kuinka monta osallistujaa per ryhmä lopullisessa aineistossa on:
length(unique(robo_final$ID))
length(unique(subset(robo_final, Group==0)$ID))
length(unique(subset(robo_final, Group==1)$ID))

# Tarkista P300-vasteen keskiarvot ja -hajonnat kummallekin ryhmälle erikseen eri koetilanteissa.
# Käytä keskiarvoa hemisfäärien yli.

tapply(P300, list(Group, Gaze), mean) # keskiarvo
tapply(P300, list(Group, Gaze), sd) # keskihajonta

# Tulosta laatikkokuvio, jossa näkyy P300-vaste erikseen kummallekin ryhmälle, koetilanteelle ja hemisfäärille.
boxplot(P300~Gaze+Group+Hemisphere)

# Kuvaile havaintosi sanallisesti.

# ---

# 3. Mallin luominen

# Testaa ryhmän (Group), katseen suunnan (Gaze) ja aivopuoliskon (Hemisphere) pää- ja yhdysvaikutukset P300-vasteeseen.

library(afex)
A1 <- aov_car(P300 ~ Group + Error(ID/Gaze*Hemisphere), data=robo_final)


#---

# 4. Oletusten tarkastelu

# P300-vaste:

# Vierashavainnot:
library(dplyr)
library(rstatix)  # lataa tarvittava kirjasto, asenna tarvittaessa ensin

# Koko aineisto
robo_final %>%
  identify_outliers(P300)
# ei vierashavaintoja

# Koetilanteittan ja aivopuoliskoittain ryhmän mukaan:
robo_final %>%
  group_by(Group, Gaze, Hemisphere) %>%
  identify_outliers(P300)

# Ei vierashavaintoja.

# Jäännöstermien normaalijakautuneisuus:
library(performance)
A1_onko_norm <- check_normality(A1)
A1_onko_norm
plot(A1_onko_norm)
# Jäännöstermit ovat normaalijakautuneet.

# Sfäärisyys:
check_sphericity(A1)

# ---

# 5. Tilastollisen testin tulosten tarkastelu ja raportointi

# P300-vaste:
summary(A1)
library(effectsize)
effectsize::eta_squared(A1)
# Ryhmän päävaikutus, F(1,63)=9.20, p=.0035, eta2=0.13
# Ryhmän ja katseen suunnan yhdysvaikutus, F(1,63)=8.63, p=.0046, eta2=0.12
# Ryhmän ja hemisfäärin yhdysvaikutus, F(1,63)=7.53, p=.0079, eta2=0.11

# Näytetään ryhmän ja katseen suunnan yhdysvaikutus interaktiokuviona:
interaction.plot(Group, Gaze, P300,
                 type="b", col=c(1:6),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="Ryhmä",
                 ylab="P300",
                 main="Ryhmän ja katseen suunnan vaikutus")

# Näytetään ryhmän ja hemisfäärin yhdysvaikutus interaktiokuviona:
interaction.plot(Group, Hemisphere, P300,
                 type="b", col=c(1:6),
                 leg.bty="o",leg.bg="beige",
                 lwd=2, pch=c(18,18),
                 xlab="Hemisfääri",
                 ylab="P300",
                 main="Ryhmän ja hemisfäärin vaikutus")
