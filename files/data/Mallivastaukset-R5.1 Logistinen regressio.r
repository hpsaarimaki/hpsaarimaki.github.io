# Logistinen regressio
# Harjoituksia
# Heini Saarim�ki 3.10.2022

# -----

# 1. Aineiston lataaminen ja valmistelu

# Ladataan aineisto
titanic <- read.csv('https://bit.ly/PSY204_titanic', header=T, sep=";", na.strings=c(""))

# Tarkistetaan faktoreiden koodaus:
summary(titanic)

# Vaihtoehtoinen tapa:
is.factor(titanic$Survived)
is.factor(titanic$Sex)
is.factor(titanic$Pclass)
is.factor(titanic$Cabin)
is.factor(titanic$Embarked)

# Muutetaan kategoriset muuttujat faktoreiksi:
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- factor(titanic$Sex)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Cabin <- factor(titanic$Cabin)
titanic$Embarked <- factor(titanic$Embarked)

# Tarkastellaan tietokehyst� uudestaan:
summary(titanic)

# Tarkistetaan kategoristen muuttujien dummy-koodaus:
contrasts(titanic$Sex) 
contrasts(titanic$Embarked)
contrasts(titanic$Pclass)

# Puuttuvien havaintojen tarkastaminen:
summary(titanic)

# Ik� puuttuu monilta
library(Amelia)
missmap(titanic, main="Puuttuvat vs havaitut arvot")

# Hytti puuttuu monilta, poistetaan koko sarake:
titanic$Cabin <- NULL

# Korvataan puuttuvat i�n arvot keskiarvolla:
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age,na.rm=T)

# Poistetaan puuttuvat l�ht�sataman arvot:
titanic <- titanic[complete.cases(titanic), ]

# -----

# 2. Kuvailevat tulokset

# Tallennetaan keski- ja hajontaluvut jatkuville muuttujille:
library(psych)
keskiluvut <- describe(titanic[4:7])
keskiluvut
write.csv(keskiluvut, 'titanic_keskiluvut.csv')

# Tallennetaan frekvenssit kategorisille muuttujille:
frekvenssit <- summary(titanic[c(1:3, 8)])
frekvenssit
write.csv(frekvenssit, 'titanic_frekvenssit.csv')


# -----

# 3. Mallin sovittaminen

# Jaetaan aineisto opetus- ja testausaineistoon:
opetus <- titanic[1:800,]
testaus <- titanic[801:889,]

# Sovitetaan malli:
malli <- glm(Survived ~ Pclass + Sex + Age + SibSp + ParCh + Fare + Embarked, family=binomial, data=opetus)

# -----

# 4. Oletusten tarkastelu

# Otoskoko on riitt�v�:
nrow(opetus)

# Multikollineaarisuus jatkuville muuttujille:
corr.test(opetus[4:7])
# Korrelaatioita on, mutta ne ovat melko pieni� (suurin r=.41)
library(car)
vif(malli)
# Ei h�lytt�v�� multikollinearisuutta

# -----

# 5. Mallin tulkinta

# 5.1 Yksitt�isten selitt�jien merkitys
summary(malli)

lippu_ja_luokka <- aov(Fare ~ Pclass, data=opetus)
summary(lippu_ja_luokka)
boxplot(Fare ~ Pclass, data=opetus)

# -

# 5.2 Mallin sopivuus
anova(malli, test="Chisq")

# -

# 5.3 Mallin selitysaste

# Pseudo-R2:n estimointi:
nollamalli <- glm(Survived ~ 1, data=opetus, family="binomial")
1-logLik(malli) / logLik(nollamalli)

# Nagelkerken pseudo-R2:
library(fmsb)
NagelkerkeR2(malli)

# -

# 5.4 Mallin ennustustarkkuus

ennusteet <- predict(malli,newdata=subset(testaus,select=c(2,3,4,5,6,7,8)),type='response')
ennusteet <- ifelse(ennusteet > 0.5,1,0)

luokitteluvirheet <- mean(ennusteet != testaus$Survived, na.rm=T)
print(paste('Tarkkuus:',1-luokitteluvirheet))

# -

# 5.5 Mallin tarkkuus ja herkkyys

# Piirret��n ROC-k�yr�:
library(ROCR)
pr <- prediction(ennusteet, testaus$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# Lasketaan ROC-k�yr�n alapuolelle j��v� pinta-ala:
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# Arvo on l�hell� yht�, eli mallin ennustustarkkuus on hyv�.

# Ristiintaulukointi:
testaus$Predicted <- factor(ennusteet)  
ennustejakauma <-prop.table(table(testaus$Predicted,
                                  testaus$Survived),2)*100
round(ennustejakauma, 2)

#Kuvio:
barplot(ennustejakauma, legend=TRUE, 
        xlab="Todellinen luokka", ylab="Ennuste")

# -----

# 6. Harjoituksia

# Teht�v� 1:

# Luodaan uusi malli:
malli_uusi <- glm(Survived ~ Pclass + ParCh, family=binomial, data=opetus)

# -

# Teht�v� 2:

# Otoskoko on riitt�v�:
nrow(opetus)

# VIF-arvot saadaan my�s kategorisille muuttujille:
vif(malli_uusi)

# -

# Teht�v� 3:

# Yksitt�iset ennustajat:
summary(malli_uusi)
# Molemmat muuttujat ennustavat selviytymist�.
# Mit� alempi lippuluokka, sit� ep�todenn�k�isempi selviytyminen.
# Jos laivalla mukana vanhemmat tai lapset, sit� todenn�k�isempi selviytyminen

# Mallin sopivuus:
anova(malli_uusi, test="Chisq")

# Nagelkerken pseudo-R2:
NagelkerkeR2(malli)

# Ennustustarkkuus:
ennusteet_uusi <- predict(malli_uusi,newdata=subset(testaus,select=c(2,6)),type='response')
ennusteet_uusi <- ifelse(ennusteet_uusi > 0.5,1,0)
luokitteluvirheet_uusi <- mean(ennusteet_uusi != testaus$Survived, na.rm=T)
print(paste('Tarkkuus:',1-luokitteluvirheet_uusi))

# Piirret��n ROC-k�yr�:
pr <- prediction(ennusteet_uusi, testaus$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# Lasketaan ROC-k�yr�n alapuolelle j��v� pinta-ala:
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# Arvo on l�hemp�n� 0.5 kuin yht�, eli ennustustarkkuus ei ole kovin hyv�.

# Ristiintaulukointi:
testaus$Predicted_uusi <- factor(ennusteet_uusi)  
ennustejakauma_uusi <-prop.table(table(testaus$Predicted_uusi,
                                  testaus$Survived),2)*100
round(ennustejakauma_uusi, 2)

#Kuvio:
barplot(ennustejakauma_uusi, legend=TRUE, 
        xlab="Todellinen luokka", ylab="Ennuste")

# Teht�v� 5:

# Mallien vertailu
library(AICcmodavg)
aictab(cand.set = list(malli, malli_uusi), modnames = c("malli", "malli_uusi") )

# Alkuper�inen malli sopii aineistoon paremmin.


