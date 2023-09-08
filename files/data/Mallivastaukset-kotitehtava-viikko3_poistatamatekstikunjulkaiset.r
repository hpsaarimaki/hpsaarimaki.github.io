# PSY204
# Kotitehtävät viikko 3
# Mallivastaukset
# Heini Saarimäki 8.9.2023

# -----

# 1. Aineiston valmistelu

# Asetetaan työskentelykansio
setwd('C:/Users/sbhesa/Documents/Opetus/2023-2024/PSY204 - syksy 2023/Viikko 3/')

# Ladataan aineisto:
peltola <- read.csv("https://hpsaarimaki.github.io/files/data/peltola_clean.csv", header=T)

# Tarkastellaan aineistoa:
summary(peltola)

# Muutetaan faktorit faktoreiksi:
peltola$ID <- factor(peltola$ID)
peltola$face_age <- factor(peltola$face_age)
peltola$facial_expression <- factor(peltola$facial_expression)
peltola$condition <- factor(peltola$condition)

# Tarkastetaan aineisto:
summary(peltola)
# Faktorit on nyt koodattu oikein.

# Kiinnitetään aineisto:
attach(peltola)

# 2. Kuvailevat tulokset

# Laatikkokuvio:
boxplot(N170 ~ condition)

# Ladataan kirjasto 'psych'
library(psych)

# Otetaan keskiluvut ja tallennetaan ne tiedostoon:
keskiluvut <- describe(peltola)
write.csv(keskiluvut, "peltola_keskiluvut.csv")

# ---

# 3. Mallin luominen

# Ladataan tarvittava kirjasto:
library(afex)

# Rakennetaan malli:
A1 <- aov_car(N170 ~ Error(ID/condition*face_age*facial_expression), data=peltola)

# ---

# 4. Oletusten tarkastelu

# Ladataan tarvittava kirjasto:
library(rstatix)

# Vierashavainnot:

# Koko aineisto
peltola %>%
  identify_outliers(N170)
# ei vierashavaintoja

# Koetilanteittain:
peltola %>%
  group_by(condition, face_age, facial_expression) %>%
  identify_outliers(N170)
# ei vierashavaintoja

# N170-muuttujassa ei ole merkitseviä vierashavaintoja.

# Normaalijakautuneisuus:

# Ladataan tarvittava kirjasto:
library(performance)

# Malli A1:
A1_is_norm <- check_normality(A1)
A1_is_norm
# residuaalit ei normaalisti jakautuneet
# Tarkastellaan lisää:
plot(A1_is_norm)
plot(A1_is_norm, type="qq")
# jäännöstermien jakauma on vino

# Varianssien yhtäsuuruus:
# Tarvittaessa saataisiin esim. tällä funktiolla:
check_sphericity(A1)
# Mallin riippumattomat muuttujat (condition, face_age ja facial_expression)
# ovat kuitenkin kaksiluokkaisia, joten sfäärisyysoletus pätee aina.
# Sfäärisyystesti lasketaan aina kaikille parittaisille luokkien eroille,
# joten kaksiluokkaiselle muuttujalle sen laskemisesta ei ole hyötyä.


# ---

# 5. Tilastollisen testin tulosten tarkastelu ja raportointi

# Varianssianalyysin tulokset:
summary(A1)
anova(A1, es="pes")

# Vain päävaikutukset ovat merkitseviä:
# Hormonikonditio (OT vs plasebo), F(1,37)=7.31, p<.05, pes = 0.16)
# Kasvojen ikä (aikuinen vs lapsi), F(1,37)=38.62, p<.001, pes = 0.51)
# Kasvonilme (iloinen vs surullinen), F(1,37)=43.18, p<.001, pes = 0.54)

# Post hoc -testejä ei tarvita, koska kaikki riippumattomat muuttujat
# ovat kaksiluokkaisia. Tällöin koetilanteiden ero näkyy suoraan ANOVAsta.

# Yhteisvaikutukset eivät olleet tilastollisesti merkitseviä,
# joten interaktiokuviota ei tehdä.

# -----
