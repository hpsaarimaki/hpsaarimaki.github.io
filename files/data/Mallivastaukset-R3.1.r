# PSY204
# R-harjoitusmoniste R3.1
# Mallivastaukset
# Heini Saarimäki 10.9.2024

# -----

# Aseta työskentelykansio
setwd('C:/Users/sbhesa/OneDrive - TUNI.fi/Opetus/2024-2025/PSY.204 syksy 2024/Harjoitukset/')

# ---

# 1. Aineiston valmistelu

# Tämä osio kopioitu suoraan harjoitusmonisteen 2.1 vastauksista.
# Aineiston tarkastelut tehty t-testien yhteydessä, joten niitä ei tässä tehdä uudestaan.
# Äärimmäisen poikkeavia havaintoja ei ollut missään muuttujassa, joten valmisteluista
# tehdään vain kategoristen muuttujien muuttaminen faktoreiksi.

# Ladataan aineisto:
deprivation <- read.csv('https://bit.ly/PSY204_deprivation')

# Muutetaan kategoriset muuttujat ID, age, education ja ht_group faktoreiksi:
deprivation$ID <- factor(deprivation$ID)
deprivation$age <- factor(deprivation$age)
deprivation$education <- factor(deprivation$education)
deprivation$ht_group <- factor(deprivation$ht_group)

# Tarkistetaan aineisto:
summary(deprivation)

# Kun olet valmistellut aineiston, sen voi kiinnittää:
attach(deprivation)

# ---

# 2. Yksisuuntainen riippumattomien otosten ANOVA

# 2.1 Merkkikoe

# Oletusten tarkastelu:

# Ryhmäkoko eri ryhmissä:
summary(education)
# Ryhmäkoko on 13-18 osallistujaa, mutta harjoituksen vuoksi käytetään
# silti yksisuuntaista ANOVAa.

# Merkkikokeen jakauma eri koulutusryhmissä:
hist(subset(deprivation, education == "1")$digitsymbol_1) 
hist(subset(deprivation, education == "2")$digitsymbol_1)
hist(subset(deprivation, education == "3")$digitsymbol_1)

# Shapiro-Wilk:
shapiro.test(subset(deprivation, education == "1")$digitsymbol_1) 
shapiro.test(subset(deprivation, education == "2")$digitsymbol_1)
shapiro.test(subset(deprivation, education == "3")$digitsymbol_1)
# Merkkikokeen pistemäärä on normaalijakautunut kaikissa muissa ryhmissä paitsi
# korkeakoulutetuilla.

# Levenen testi:
library(car)
leveneTest(digitsymbol_1 ~ education)
# Varianssit ovat yhtä suuret.

# Yksisuuntainen ANOVA koulutusryhmien eroille merkkitestissä:
A1 <- aov(digitsymbol_1 ~ education, data=deprivation)

# Tulostetaan ANOVA:n tulokset:
summary(A1)

# Efektikoko:
library(effectsize)
eta_squared(A1)

# Ei merkitseviä eroja, F(2,44)=2.16, p=.128, eta^2=0.09.

# Koska normaalijakaumaoletus ei toteutunut kolmannen ryhmän osalta,
# tarkistetaan vielä tulokset Kruskal-Wallisin testillä:
kruskal.test(digitsymbol_1 ~ education, data=deprivation)

# Koska parametrinen ja epäparametrinen testi antavat saman tuloksen,
# voimme todeta, että koulutustaso ei todennäköisesti vaikuta merkkitestin
# perussuoriutumiseen. Yleensä raportoidaan parametrisen testin tulos, mutta
# tulososiossa voi myös kommentoida, että epäparametrinen testi antoi saman
# tuloksen.

# Visualisoidaan tulokset:
boxplot(digitsymbol_1 ~ education, xlab="Koulutusryhmä", 
        ylab="Merkkitestin pistemäärä",
        main="Merkkitestin pistemäärä eri koulutusryhmissä", 
        col="grey")

# 2.2 Bentonin testi

# Oletusten tarkastelu:

# Bentonin virhepisteiden jakauma eri koulutusryhmissä:
hist(subset(deprivation, education == "1")$bentonerror_1) 
hist(subset(deprivation, education == "2")$bentonerror_2)
hist(subset(deprivation, education == "3")$bentonerror_3)

# Shapiro-Wilk:
shapiro.test(subset(deprivation, education == "1")$bentonerror_1) 
shapiro.test(subset(deprivation, education == "2")$bentonerror_2)
shapiro.test(subset(deprivation, education == "3")$bentonerror_3)
# Merkkikokeen pistemäärä on normaalijakautunut kaikissa muissa ryhmissä paitsi
# toisen asteen koulutuksen saaneilla.

# Levenen testi:
leveneTest(bentonerror_1 ~ education)
# Varianssit ovat yhtä suuret.

# Yksisuuntainen ANOVA koulutusryhmien eroille merkkitestissä:
A2 <- aov(bentonerror_1 ~ education, data=deprivation)

# Tulostetaan ANOVA:n tulokset:
summary(A2)

# Efektikoko:
eta_squared(A2)

# Ei merkitseviä eroja, F(2,44)=0.024, p=.976, eta^2=0.001.

# Koska normaalijakaumaoletus ei toteutunut kolmannen ryhmän osalta,
# tarkistetaan vielä tulokset Kruskal-Wallisin testillä:
kruskal.test(bentonerror_1 ~ education, data=deprivation)

# Koska parametrinen ja epäparametrinen testi antavat saman tuloksen,
# voimme todeta, että koulutustaso ei todennäköisesti vaikuta Bentonin testin
# perussuoriutumiseen. 

# Visualisoidaan tulokset:
boxplot(bentonerror_1 ~ education, xlab="Koulutusryhmä", 
        ylab="Bentonin testin pistemäärä",
        main="Bentonin testin pistemäärä eri koulutusryhmissä", 
        col="grey")




# ---

# 3. Kaksisuuntainen riippumattomien otosten ANOVA

# 3.1 Merkkikoe

# Ryhmäkoko eri ryhmissä:
table(age,ht_group)
# Ryhmäkoko on 10-15 osallistujaa.

# Merkkikokeen jakauma eri ikäryhmissä:
hist(subset(deprivation, age == "1")$digitsymbol_1) 
hist(subset(deprivation, age == "2")$digitsymbol_1)

# Shapiro-Wilk:
shapiro.test(subset(deprivation, age == "1")$digitsymbol_1) 
shapiro.test(subset(deprivation, age == "2")$digitsymbol_1)
# Merkkikokeen pistemäärä on normaalijakautunut molemmissa ikäryhmissä.

# Merkkikokeen jakauma eri hormonihoitoryhmissä:
hist(subset(deprivation, ht_group == "user")$digitsymbol_1) 
hist(subset(deprivation, ht_group == "control")$digitsymbol_1)

# Shapiro-Wilk:
shapiro.test(subset(deprivation, ht_group == "user")$digitsymbol_1) 
shapiro.test(subset(deprivation, ht_group == "control")$digitsymbol_1)
# Merkkikokeen pistemäärä on normaalijakautunut molemmissa hormonihoitoryhmissä.

# Levenen testi:
library(car)
leveneTest(digitsymbol_1 ~ ht_group*age)
# Varianssit ovat yhtä suuret.

# Kaksisuuntainen ANOVA:
A3 <- aov(digitsymbol_1 ~ ht_group*age, data=deprivation)

# Tulostetaan ANOVA:n tulokset:
summary(A3)
# Vain yhdysvaikutus on merkitsevä.

# Efektikoko:
library(effectsize)
eta_squared(A3)

# Hormonihoidon ja iän yhdysvaikutus on merkitsevä, F(1,43)=6.81, p=.012, eta2=0.14).
# Hormonihoidon ja iän päävaikutukset eivät ole merkitseviä.

# Tarkastellaan tuloksia interaktiokuvion avulla:
interaction.plot(ht_group, age, digitsymbol_1)

# Verrokkiryhmässä pistemäärä on korkeampi vanhemmilla osallistujilla.
# Hormonihoitoryhmässä pistemäärä on korkeampi nuoremmilla osallistujilla.

# -

# 3.2 Bentonin virhepisteet

# Bentonin jakauma eri ikäryhmissä:
hist(subset(deprivation, age == "1")$bentonerror_1) 
hist(subset(deprivation, age == "2")$bentonerror_1)

# Shapiro-Wilk:
shapiro.test(subset(deprivation, age == "1")$bentonerror_1) 
shapiro.test(subset(deprivation, age == "2")$bentonerror_1)
# Bentonin pistemäärä on normaalijakautunut molemmissa ikäryhmissä.

# Bentonin jakauma eri hormonihoitoryhmissä:
hist(subset(deprivation, ht_group == "user")$bentonerror_1) 
hist(subset(deprivation, ht_group == "control")$bentonerror_1)

# Shapiro-Wilk:
shapiro.test(subset(deprivation, ht_group == "user")$bentonerror_1) 
shapiro.test(subset(deprivation, ht_group == "control")$bentonerror_1)
# Bentonin pistemäärä on normaalijakautunut molemmissa hormonihoitoryhmissä.

# Levenen testi:
leveneTest(bentonerror_1 ~ ht_group*age)
# Varianssit ovat yhtä suuret.

# Kaksisuuntainen ANOVA:
A4 <- aov(bentonerror_1 ~ ht_group*age, data=deprivation)

# Tulostetaan ANOVA:n tulokset:
summary(A4)
# Vain hormonihoidon päävaikutus on merkitsevä.

# Efektikoko:
eta_squared(A4)

# Hormonihoidon ja iän yhdysvaikutus on merkitsevä, F(1,43)=8.85, p=.005, eta2=0.17).
# Iän päävaikutus ei ole merkitsevä.
# Hormonihoidon ja iän yhdysvaikutus ei ole merkitsevä.

# Tarkastellaan tuloksia laatikkokuvion avulla:
boxplot(bentonerror_1 ~ ht_group)

# Verrokkiryhmässä virhepistemäärä on korkeampi kuin hormonihoidon käyttäjäryhmässä.

# ---
