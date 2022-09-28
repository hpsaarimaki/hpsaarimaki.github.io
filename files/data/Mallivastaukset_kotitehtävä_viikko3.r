# PSY204
# Kotiteht�v�t 2
# Mallivastaukset
# Heini Saarim�ki 16.9.2022

# -----

# 1. Aineiston valmistelu

# Asetetaan ty�skentelykansio
setwd('C:/Users/sbhesa/Documents/Opetus/2022-2023/PSY204 - syksy 2022/Materiaalit/Peltola et al 2018/')

# Ladataan aineisto:
peltola <- read.csv("https://hpsaarimaki.github.io/files/peltola/peltola_clean.csv", header=T)

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

# Kiinnitet��n aineisto:
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

# N170-muuttujassa ei ole merkitsevi� vierashavaintoja.

# Normaalijakautuneisuus:

# Ladataan tarvittava kirjasto:
library(performance)

# Malli A1:
A1_is_norm <- check_normality(A1)
A1_is_norm
# residuaalit ei normaalisti jakautuneet
# Tarkastellaan lis��:
plot(A1_is_norm)
plot(A1_is_norm, type="qq")
# j��nn�stermien jakauma on vino

# Varianssien yht�suuruus:
# Tarvittaessa saataisiin esim. t�ll� funktiolla:
check_sphericity(A1)
# Mallin riippumattomat muuttujat (condition, face_age ja facial_expression)
# ovat kuitenkin kaksiluokkaisia, joten sf��risyysoletus p�tee aina.
# Sf��risyystesti lasketaan aina kaikille parittaisille luokkien eroille,
# joten kaksiluokkaiselle muuttujalle sen laskemisesta ei ole hy�ty�.


# ---

# 5. Tilastollisen testin tulosten tarkastelu ja raportointi

# Varianssianalyysin tulokset:
summary(A1)
anova(A1, es="pes")

# Vain p��vaikutukset ovat merkitsevi�:
# Hormonikonditio (OT vs plasebo), F(1,37)=7.31, p<.05, pes = 0.16)
# Kasvojen ik� (aikuinen vs lapsi), F(1,37)=38.62, p<.001, pes = 0.51)
# Kasvonilme (iloinen vs surullinen), F(1,37)=43.18, p<.001, pes = 0.54)

# Post hoc -testej� ei tarvita, koska kaikki riippumattomat muuttujat
# ovat kaksiluokkaisia. T�ll�in koetilanteiden ero n�kyy suoraan ANOVAsta.

# Yhteisvaikutukset eiv�t olleet tilastollisesti merkitsevi�,
# joten interaktiokuviota ei tehd�.

# -----