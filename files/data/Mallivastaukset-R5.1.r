# PSY.204 syksy 2024
# Mallivastaukset
# Harjoitusmoniste R5.1
# Heini Saarimäki

# ---

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Harjoitukset/')

# Lataa tarvittavat kirjastot
library(openxlsx)
library(dplyr)

# ---

# 1. Aineiston valmistelu

# Ladataan aineisto
tunteet <- read.xlsx('tunteet.xlsx')

# Tarkista aineisto
summary(tunteet)

# Kategoriset muuttujat faktoreiksi
tunteet$ID <- factor(tunteet$ID)
tunteet$Age.group <- factor(tunteet$Age.group)

# Puuttuvat arvot koodattu valmiiksi NA:lla
# Puuttuvia arvoja on paljon

# Poistetaan ICU-kyselyä vastaavat sarakkeet:
library(dplyr)
tunteet <- tunteet %>%
  select(-starts_with("ICU"))

# Käännetään käänteiset väittämät:

# PNDL: väittämät PNDL2, PNDL5, PNDL7, PNDL9
tunteet$PNDL2.k <- 5-tunteet$PNDL2
tunteet$PNDL5.k <- 5-tunteet$PNDL5
tunteet$PNDL7.k <- 5-tunteet$PNDL7
tunteet$PNDL9.k <- 5-tunteet$PNDL9

# EmQue: ei käännettäviä väittämiä

# EAQ: väittämät EAQ1 EAQ2 EAQ3 EAQ4 EAQ7 EAQ8 EAQ9 EAQ10 EAQ11 
# EAQ13 EAQ15 EAQ19 EAQ20 EAQ21 EAQ22 EAQ24 EAQ25 EAQ26 EAQ29 EAQ30
# Luodaan lista käännettävistä muuttujista:
EAQ_käännetään <- c("EAQ01", "EAQ02", "EAQ03", "EAQ04", "EAQ07", "EAQ08", "EAQ09", "EAQ10", "EAQ11", "EAQ13", "EAQ15", "EAQ19", "EAQ20", "EAQ21", "EAQ22", "EAQ24", "EAQ25", "EAQ26", "EAQ29", "EAQ30")
# Luodaan kaikille näille uusi käännetty muuttuja
tunteet <- tunteet %>%
  mutate(across(all_of(EAQ_käännetään), ~ 4 - ., .names = "{.col}.k"))


# ---

# 2. Puuttuvat tiedot

# Koko aineistossa:
sum(complete.cases(tunteet))
# Koko aineisto on 50 osallistujalta.

# PNDL-mittari:

summary(tunteet)
# Puuttuvia tietoja on 4-5 osallistujalta.
# Tutkitaan, missä puuttuvat tiedot ovat:
tunteet %>%
  filter(if_any(5:14, is.na)) %>%
  select(ID)
# Puuttuvat osallistujat:
# TUN003: kaikki
# TUN067: PNDL6
# TUN068: PNDL3
# TUN101: kaikki
# TUN102: kaikki
# TUN103: kaikki

# Poistetaan TUN003, TUN101, TUN102, TUN103:
tunteet <- tunteet[-c(2, 75, 76, 77),]

# Korvataan PNDL-kyselyn puuttuvat arvot osallistujan vastausten keskiarvolla - huom. käänteiset väittämät
tunteet[which(tunteet$ID=="TUN067"), "PNDL6"] <- mean(as.numeric(tunteet[which(tunteet$ID=="TUN067"), c("PNDL1", "PNDL2.k", "PNDL3", "PNDL4", "PNDL5.k",
                                                                                             "PNDL7.k", "PNDL8", "PNDL9.k", "PNDL10")]))
tunteet[which(tunteet$ID=="TUN068"), "PNDL3"] <- mean(as.numeric(tunteet[which(tunteet$ID=="TUN068"), c("PNDL1", "PNDL2.k", "PNDL4", "PNDL5.k",
                                                                                                        "PNDL6", "PNDL7.k", "PNDL8", "PNDL9.k", "PNDL10")]))
# -

# EmQue-mittari:
summary(tunteet)
# Puuttuvia tietoja on 6-7 osallistujalta.

# Tutkitaan, missä puuttuvat tiedot ovat:
tunteet %>%
  filter(if_any(15:32, is.na)) %>%
  select(ID)

# Puuttuvat tiedot:
# TUN005
# TUN006
# TUN018
# TUN067
# TUN068
# TUN083
# TUN095
# TUN111

# Määritä rajaksi puuttuvien osuus yli 50%
puuttuvien_osuus <- rowMeans(is.na(tunteet[15:32]))
puuttuu_yli_50 <- tunteet[puuttuvien_osuus > 0.5, 'ID'] 
puuttuu_yli_50

# Yli 50% havainnoista puuttuu osallistujilta:
# TUN005, TUN006, TUN18, TUN67, TUN083, TUN095

# --

# EAQ-mittari:
summary(tunteet)

# Yksittäiset puuttuvat vastaukset, etsitään ne ensin:
# 19.k: rivi 90
# 21k.: rivi 47
# 22.k: rivi 27
# 24.k: rivi 19, 22, 87
yksittäiset_puuttuvat <- c("TUN028", "TUN031", "TUN036", "TUN061", "TUN116", "TUN120")
kaikki_puuttuu <- tunteet %>%
  filter(if_any(33:62, is.na)) %>%
  filter(!ID %in% yksittäiset_puuttuvat) %>%
  select(ID)

# Vaihtoehtoinen tapa etsiä ID:t, joilta puuttuu kaikki vastaukset tietyissä sarakkeissa:
kaikki_puuttuu <- tunteet %>%
  filter(if_all(33:62, is.na)) %>%
  select(ID)

# Tehdään katoanalyysi.

# Puuttuvia tietoja on 29 osallistujalta. Poistetaan yllä olevat osallistujat listasta, muille
# annetaan tieto puuttuvasta datasta.

# Luo uusi muuttuja:
tunteet$eaq_puuttuu <- 0
tunteet$eaq_puuttuu[tunteet$ID %in% kaikki_puuttuu$ID] <- 1

# Eroaako ikä:
t.test(tunteet$Age ~ tunteet$eaq_puuttuu)
# eroaa

# Eroaako sukupuoli:
chisq.test(tunteet$Gender, tunteet$eaq_puuttuu)
# ei

# Entä ikäryhmä:
chisq.test(tunteet$Age.group, tunteet$eaq_puuttuu)
plot(tunteet$Age.group, tunteet$eaq_puuttuu)

# 3. Summamuuttujien luominen

# Kiinnitä aineisto työskentelyn helpottamiseksi:
attach(tunteet)

# PNDL summamuuttujat
tunteet$Social.loneliness <- rowSums(cbind(PNDL1, PNDL2.k, PNDL3, PNDL4, PNDL5.k))
tunteet$Emotional.loneliness <- rowSums(cbind(PNDL6, PNDL7.k, PNDL8, PNDL9.k, PNDL10))
tunteet$PNDL_total <- rowSums(tunteet[88:89])

# EMQUE summamuuttujat
tunteet$EPCON <- rowMeans(cbind(EMQUE01, EMQUE04, EMQUE07, EMQUE10, EMQUE13, EMQUE16), na.rm=T)
tunteet$EPATT <- rowMeans(cbind(EMQUE03, EMQUE06, EMQUE09, EMQUE15, EMQUE18, EMQUE12), na.rm=T)
tunteet$EPPRO <- rowMeans(cbind(EMQUE02, EMQUE05, EMQUE08, EMQUE11, EMQUE14, EMQUE17), na.rm=T)
# Anna summamuuttujille arvo NA, jos puuttui yli 50% vastauksista:
tunteet$EPCON[puuttuu_yli_50] <- NA
tunteet$EPATT[puuttuu_yli_50] <- NA
tunteet$EPPRO[puuttuu_yli_50] <- NA

# EAQ summamuuttujat
tunteet$ADIF <- rowMeans(cbind(EAQ01.k, EAQ07.k, EAQ13.k, EAQ19.k, EAQ24.k, EAQ29.k, EAQ30.k), na.rm=T)
tunteet$ATALK <- rowMeans(cbind(EAQ02.k, EAQ08.k, EAQ14), na.rm=T)
tunteet$AHIDE <- rowMeans(cbind(EAQ03.k, EAQ09.k, EAQ15.k, EAQ20.k, EAQ25.k), na.rm=T)
tunteet$ABOD <- rowMeans(cbind(EAQ04.k, EAQ10.k, EAQ16, EAQ21.k, EAQ26.k), na.rm=T)
tunteet$AOTH <- rowMeans(cbind(EAQ05, EAQ11.k, EAQ17, EAQ22.k, EAQ27), na.rm=T)
tunteet$AOWN <- rowMeans(cbind(EAQ06, EAQ12, EAQ18, EAQ23, EAQ28), na.rm=T)

# Kiinnitetään aineisto uudestaan
attach(tunteet)

# ---

# 4. Cronbachin alfa

# Voit käyttää Cronbachin alfaa joko kirjastosta 'ltm' tai kirjastosta 'psych:

# Esim. Social loneliness:

# Kirjastosta psych:
library(psych)
alpha(cbind(PNDL1, PNDL2.k, PNDL3, PNDL4, PNDL5.k))
# Tarkista arvo kohdasta 'raw_alpha': 0.76

# Kirjastosta ltm:
library(ltm)
cronbach.alpha(cbind(PNDL1, PNDL2.k, PNDL3, PNDL4, PNDL5.k))
# Tarkista arvo kohdasta 'raw_alpha'

# Tarkista vastaavasti Cronbachin alfat kaikille summamuuttujille, tässä käytetään kirjastoa ltm:
cronbach.alpha(cbind(PNDL6, PNDL7.k, PNDL8, PNDL9.k, PNDL10)) # emot. yksinäisyys: 0.63
cronbach.alpha(cbind(EMQUE01, EMQUE04, EMQUE07, EMQUE10, EMQUE13, EMQUE16), na.rm=T) # EPCON: 0.70
cronbach.alpha(cbind(EMQUE03, EMQUE06, EMQUE09, EMQUE15, EMQUE18, EMQUE12), na.rm=T) # EPATT: 0.83
cronbach.alpha(cbind(EMQUE02, EMQUE05, EMQUE08, EMQUE11, EMQUE14, EMQUE17), na.rm=T) # EPPRO: 0.60
cronbach.alpha(cbind(EAQ01.k, EAQ07.k, EAQ13.k, EAQ19.k, EAQ24.k, EAQ29.k, EAQ30.k), na.rm=T) # ADIF: 0.74
cronbach.alpha(cbind(EAQ02.k, EAQ08.k, EAQ14), na.rm=T) # ATALK: 0.65
cronbach.alpha(cbind(EAQ03.k, EAQ09.k, EAQ15.k, EAQ20.k, EAQ25.k), na.rm=T) # AHIDE: 0.79
cronbach.alpha(cbind(EAQ04.k, EAQ10.k, EAQ16, EAQ21.k, EAQ26.k), na.rm=T) # ABOD: 0.81
cronbach.alpha(cbind(EAQ05, EAQ11.k, EAQ17, EAQ22.k, EAQ27), na.rm=T) # AOTH: 0.73
cronbach.alpha(cbind(EAQ06, EAQ12, EAQ18, EAQ23, EAQ28), na.rm=T) # AOWN: 0.79

# ---

# 5. Kategoristen muuttujien luominen:

# Luodaan muuttuja Loneliness, ja annetaan sille luokka PNDL-total-muuttujan mediaaniin perustuen:
tunteet$Loneliness <- 'high' 
tunteet$Loneliness[which(tunteet$PNDL_total < median(tunteet$PNDL_total))] <- 'low'
tunteet$Loneliness <- factor(tunteet$Loneliness)

# Tarkista frekvenssit:
summary(tunteet$Loneliness)
# korkea yksinäisyys: 44, matala yksinäisyys: 42

# Kiinnitä tietokehys:
attach(tunteet)


# ---

# 6.

# 6.1 Pearsonin khiin neliö -yhteensopivuustesti

# Tarkista ensin frekvenssit:
summary(factor(tunteet$Age.group))

# Tallennetaan havaitut frekvenssit:
observed_counts <- c("1" = 27, "2" = 26, "3" = 24, "4" = 13)  # Replace with your actual counts

# Tallennetaan odotetut frekvenssit:
expected_counts <- c(22.5, 22.5, 22.5, 22.5)

# Khiin neliö -testi:
chisq.test(observed_counts, p = expected_counts / sum(expected_counts))

# -

# 6.2 Khiin neliö -riippumattomuustesti

# Ikäryhmän ja sukupuolen yhteys

# Ristiintaulukointi:
table(Age.group, Gender)

# Ristiintaulukointi prosenteilla:
round(prop.table(table(Age.group, Gender), margin=1)*100, 1)

# Khiin neliö-testi:
chisq.test(Age.group, Gender)
# Ikäryhmien välillä ei ole eroa sukupuolessa, X2(3)=4.89, p=.180

# Yhden luokan frekvenssi oli 4, joten käytetään Fisherin tarkkaa testiä:
fisher.test(Age.group, Gender)
# p=.190

# -

# Sukupuolen ja yksinäisyyden yhteys

# Ristiintaulukointi
table(Loneliness, Gender)

# Khiin neliö-testi:
chisq.test(Loneliness, Gender)
# Yksinäisyysryhmien välillä ei ole sukupuolieroja, X2(1)=0.167, p=.682

# Kuvio:
plot(Loneliness, Gender, xlab="Loneliness", ylab="Gender")
