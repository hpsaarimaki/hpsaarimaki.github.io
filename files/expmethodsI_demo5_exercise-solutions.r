# Demo 5
# Example solutions
# 16.10.2014, HS

# ---


# 1. Categorical associations and nonparametric tests

# ---

# 1.1 Load in and prepare data

cellphone <- read.csv('http://becs.aalto.fi/~heikkih3/cellphone.csv', header=T)

head(cellphone)
summary(cellphone)

# Missing values are already coded as NA's
# No other suspective codings of values

# Recode agegroup and rfjarj as factors:

cellphone$ageggroup <- factor(cellphone$ageggroup)
cellphone$rfjarj <- factor(cellphone$rfjarj)

# Attach data frame:

attach(cellphone)

# ---

# 1.2 Normality tests

# Histograms:

# Baseline symptom variables:

layout(matrix(c(1,2,3,4),2,2))
hist(headache1)
hist(dizzines1)
hist(tiredness1)
hist(itching1)

# Symptom variables when phone is on:

layout(matrix(c(1,2,3,4),2,2))
hist(headache_on)
hist(dizzines_on)
hist(tiredness_on)
hist(itching_on)

# Symptom variables when phone is off:

layout(matrix(c(1,2,3,4),2,2))
hist(headache_off)
hist(dizziness_off)
hist(tiredness_off)
hist(itching_off)

# Note the skewness of distributions.

# Kolmogorov-Smirnov tests

ks.test(headache1, "pnorm", mean=mean(headache1, na.rm=T), sd=sd(headache1, na.rm=T))
ks.test(dizzines1, "pnorm", mean=mean(dizzines1, na.rm=T), sd=sd(dizzines1, na.rm=T))
ks.test(tiredness1, "pnorm", mean=mean(tiredness1, na.rm=T), sd=sd(tiredness1, na.rm=T))
ks.test(itching1, "pnorm", mean=mean(itching1, na.rm=T), sd=sd(itching1, na.rm=T))

# Also normality tests suggest that distributions are not normal
# (here, only for baseline, you can also check for other timepoints).

# Therefore, normality assumption is not fulfilled in the variables explored
# here, we should use nonparametric tests instead of parametric ones.

# ---

# 1.3 Chi-square tests

# Does the sex distribution in our sample represent the sex distribution of 
# the population?

# check frequencies:

table(sex)					
# ok, 50% men, 50% women
 
# test against the known population sex distribution:

chisq.test(table(sex), p=c(.5,.5))	
# obvious result: sample distribution is representative of the population

# Are there differences between men and women in age group distributions?

table(sex, ageggroup)
# there seems to be some differences:
# most women belong to the youngest group, most men to the oldest group

# test explicitly:

chisq.test(sex, ageggroup)
# yes, age distributions are different between men and women

# ---

# 1.4 Two independent samples

# Do men and women differ in experienced headache when phone is on?

# plot the distributions:

boxplot(headache_on ~ sex)
# difficult to tell, distributions look skewed

# you can try with the t-test (even though you should use non-parametric test!):

t.test(headache_on ~ sex)

# here's the proper non-parametric option: Mann-Whitney-Wilcoxon Test

wilcox.test(headache_on ~ sex)
# note that syntax is similar to t-test conventions
# result is non-significant: no sex differences in experienced headache when phone is on

# -

# Question 1

# plot distributions of dizziness when phone is on for men and women separately:

boxplot(dizzines_on ~ sex)

# use non-parametric Mann-Whitney-Wilcoxon:

wilcox.test(dizzines_on ~ sex)
# no sex differences in experienced headache when phone is on

# and take the t-test too for practice purposes

t.test(dizzines_on ~ sex)


# ---

# 1.5 Repeated samples

# Does the experience of headache change from baseline when phone is turned on?

# Wilcoxon test:

wilcox.test(headache1, headache_on, paired=T)
# no differences in experienced headache between baseline and when phone is on

# you can also check t-test to see that the syntax is again similar:

t.test(headache1, headache_on, paired=T)

# -

# Question 2

# Does the experience of tiredness change from baseline when phone is turned on?

# means:

mean(tiredness1, na.rm=T)
mean(tiredness_on)

# Wilcoxon test:

wilcox.test(tiredness1, tiredness_on, paired=T)
# no differences in experienced tiredness between baseline and when phone is on
# although the is a trend towards significance, pointing to that people
# might experience more tiredness when phone is on


# ---

# 1.6 Multiple independent samples

# Do age groups differ in expecienced tiredness when phone is on?

# take means for groups separately

tapply(tiredness_on, ageggroup, mean)
# slightly higher mean in middle agegroup, but is it significantly higher?

# take a boxplot:

boxplot(tiredness_on~ageggroup)

# take ANOVA for comparison purposes, even though we should use nonparametric test!
# (because distributions are not normal)

model.anova <- aov(tiredness_on ~ ageggroup)
summary(model.anova)

# the correct option is the nonparametric Kruskall-Wallis test:

kruskal.test(tiredness_on ~ ageggroup)
# no significant differences between agegroups in experienced tiredness when phone is on.

# -

# Question 3

# Are the age differences in experienced headache when phone is on?

# means:

tapply(headache_on, ageggroup, mean)

# boxplot:

boxplot(headache_on ~ ageggroup)

# Kruskall-Wallis:

kruskal.test(headache_on ~ ageggroup)
# no differences between agegroups in experienced headache when phone is on

# -----


# 2. Summary dataset

# ---

# 2.1 Load in, prepare and describe data

# -

# 1. Load in data:

data <- read.csv('http://becs.aalto.fi/~heikkih3/nhefs.csv', header=T) 

# -

# 2. Examine data:

summary(data)
head(data)
names(data)

# save only the variables we need into a new data frame:

data2 <- data[,-c(1,5,9,10,17,18,19)]

summary(data2)

# -

# 3. Data checking:

# (a) check that factors are coded as factors:

summary(data2)
# --> sex and df-variables are not coded as factors yet
# --> keep df-variables as numeric for now... (see later question 4a)

# code factors as factors:

data2$male <- factor(data2$male)


# -

# (b) Find the number of missing values for each variable:

summary(data2)

# extraversion: 12
# openness: 7
# ces-d: 39
# df_heart: 1

# -

# (c) Replace missing values for extraversion with mean

data2$extraversion[is.na(data2$extraversion)] <- mean(data2$extraversion, na.rm=T)

summary(data2$extraversion)

# ---

# 4. Creating a new variable that codes the number of health problems for each individual:

# create this new variable which codes the sum of health problems:
# (note that this does not work if you already coded df-variables as factors!
# recoding back to numeric is possible with function as.numeric)

data2$health_problems <- data2$dr_heart + data2$dr_cancer + data2$dr_stroke + data2$dr_diabetes + data2$dr_hypertension

summary(data2$health_problems)

# code all dr-variables as factors:

data2$dr_heart <- factor(data2$dr_heart)
data2$dr_cancer <- factor(data2$dr_cancer)
data2$dr_stroke <- factor(data2$dr_stroke)
data2$dr_diabetes <- factor(data2$dr_diabetes)
data2$dr_hypertension <- factor(data2$dr_hypertension)

summary(data2[8:13])

# seems like the data frame is ready, so attach it:

attach(data2)

# ---

# 5. Plotting data

# (a) Get some plots:

# histograms (I want them all in the same display):

layout(matrix(c(1,2,3,4),2,2))
hist(neuroticism)
hist(extraversion)
hist(openness)
hist(ces_d)

# --> ok, ces-d and neuroticism are not normally distributed!

# boxplots (again, in same display), I want to see them for each sex separately:

layout(matrix(c(1,2,3,4),2,2))
boxplot(neuroticism~male)
boxplot(extraversion~male)
boxplot(openness~male)
boxplot(ces_d~male)

# scatterplots (for numeric variables only):

names(data2)
plot(data2[c(1:4,6)])
# --> to most prominent trend is between neuroticism and ces_d

# -

# (b) plot Neuroticism and CES-D for males and females separately:

# using xyplot:

library(lattice)
xyplot(neuroticism ~ ces_d | male)

# or using coplot:

coplot(neuroticism ~ ces_d | male)

# ---

# 6. Describing data

# (a) Descriptive statistics:

# descpritive stats for personality and depression variables:

library(psych)
describe(data2[1:4])

# --> as we know based on histograms, neuroticism and ces-d are positively skewed
# this means they have more low values than high
# (i.e., high neuroticism and depression are a rarer phenomena in this sample)
# should use nonparametric tests for ces-d and neuroticism in future analyses

# -

# (b) Age distribution:

range(age)
# --> age ranges between 33 and 86

hist(age)
# --> age is quite uniformly distributed

# -----

# 2.2 Comparing groups

# 1. Chi-square tests

# (a) Sex differences in marital status

# contingency table:

table(male, marital)
# --> much more female widows, but what about percentages (relative amount)?

# show percentages in a table (row-wise percentage):

round(prop.table(table(male, marital)),2) *100
# --> also percentages show that there are more female widows

# are there statistically significant differences in marital status distributions?

chisq.test(marital, male)
# --> marital status distributions differ between men and women (chi^2(5)=36.3, p<.001)

# -

# (b) Sex distribution in sample vs population

# Chi-square test:

table(male)				# show contingency table first
round(prop.table(table(male)),2)

chisq.test(table(male), p=c(.5,.5))

# --> sex distribution in our sample is not representative for population:
# our sample has 68% women, 38% men

# -

# (c) Sex differences in number of health problems:

# using Chi-square test:

chisq.test(health_problems, male) 

# --> no sex differences in the number of health problems (chi^2(4)=3.67, p>.05)

# ---

# 2. T-tests

# (i) Sex differences in personality varibles:

# Neuroticism (remember neuroticism was not normally distributed!)

tapply(neuroticism, male, mean)
tapply(neuroticism, male, sd)
wilcox.test(neuroticism ~ male)

# --> women (M=10.6, sd=6.4) have higher values of neuroticism than men (M=8.8, sd=6.0) (p<.001)

# Extraversion:

t.test(extraversion ~ male) 

# --> no sex differences in extraversion (t(495)=-1.05, p>.05)

# Openness:

t.test(openness ~ male) 

# --> no sex differences in openness (t(472)=1.42, p>.05)

# -

# (ii) Sex differences in depression:

# remember that ces-d was not normally distributed!

tapply(ces_d, male, mean, na.rm=T)
tapply(ces_d, male, sd, na.rm=T)
wilcox.test(ces_d ~ male) 

# --> women (M=9.0, sd=8.3) have higher values in depression than men (M=7.2, sd=6.9) (p<.05)

# ---

# 3. ANOVA

# (a) Differences between classes of marital status

# (i) Differences between classes of marital status in personality:

# Neuroticism (again, remember non-normality):

kruskal.test(neuroticism ~ marital)

# --> no differences in neuroticism between classes of marital status (p>.05)

# Extraversion:

anova.1 <- aov(extraversion ~ marital)
summary(anova.1)

# --> no differences in extraversion between classes of marital status 
# F(5,596(=1.88, p>.05)

# Openness:

anova.2 <- aov(openness ~ marital)
summary(anova.2)

# --> no differences in openness between classes of marital status
# F(5,589)=2.14, p>.05)

# -

# (ii) Marital status differences in depression:

# remember non-normality again!

kruskal.test(ces_d ~ marital)

# --> no differences in depression between classes of marital status (p>.05)

# -

# (b) Differences between classes of marital status depending on sex:

# Extraversion:

anova.3 <- aov(extraversion ~ male*marital)
summary(anova.3)

# --> no sex or marital main effect, no male and marital interaction effect

# Openness:

anova.4 <- aov(openness ~ male*marital)
summary(anova.4)

# --> main effect of marital is significant when taking into account the sex differences:
# F(1,5)

# CES-D and Neuroticism: not normally distributed, so cannot use two-way ANOVA
# (there is no direct nonparametric version of two-way ANOVA, and this is out of the scope of the course)

# But if you want to run the ANOVA anyway, consider using data transformations.


# --

# (c) Orthogonal contrasts for marital variable

# Hypotheses (examples):
# Contrast 1: Married individuals are lower in depression than non-married individuals.
# Contrast 2: Those who have never been married are lower in depression than those who have but are not anymore.
# Contrast 3: Those whose spouse has died are higher in depression than those who have divorced or separated.
# Contrast 4: Divorced are higher in depression than separated.

# Contrast table:
# divorced # married # never married # not asc. # separated # widowed
#	-.25		1		-.25		0		-.25		-.25	
#	-.33		0		1		0		-.33		-.33
#	-.5		0		0		0		-.5		1
#	1		0		0		0		-1		0

# (i) Create contrast coding:

levels(marital) 

# set contrast coding as in the table 
#(add one more dummy contrast, otherwise R will add something of its own to make k-1 contrasts)

contrasts(marital) <- cbind(c(-.25,1,-.25,0,-.25,-.25), c(-.33,0,1,0,-.33,-.33), c(-.5,0,0,0,-.5,1), c(1,0,0,0,-1,0), c(0,0,0,0,0,0)) 
contrasts(marital)

# -

# (ii) Model to test the hypotheses

# NB! This is just for practice purposes since ces_d is not normally distributed!

anova.5 <- aov(ces_d ~ marital)
summary.lm(anova.5)

# --> none of the contrast is significant

# ---

# 2.3 Correlations

corr.test(data2[c(1:4,6,13)])

# --> the following correlations are significant:
# neuroticism correlates negatively with extraversion (r=-.11, p<.001) and age (r=-.18, p<.001) )
# neuroticism correlates positively with depression (r=.59, p<.001) and number of health problems (r=.12, p<.001
# extraversion correlates positively with openness (r=.2, p<.001)
# extraversion correlates negatively with depression (r=-.19, p<.001), age (r=-.21, p<.001) and number of health problems (r=-.14, p<.001)
# openness correlates negatively with age (r=-.19, p<.001) and number of health problems (r=-.15, p<.001)
# depression correlates positively with number of health problems (r=.24, p<.001)

# the only strong correlation is between depression and neuroticism

# ---


# 2.4 Regression

# --

# 1. Modelling: Neuroticism and depression

# -

# (a) Hypotheses:
# H0: Neuroticism does not predict depression.
# H1: Neuroticism predicts depression - higher neuroticism is related to higher depression.

# -

# (b) Model:

model_1 <- lm(ces_d ~ neuroticism)
summary(model_1)

# --> neuroticism is a significant predictor of depression

# -

# (c) Coefficients:

coef(model_1)

# --> ces_d ~ 1.21 + 0.73*neuroticism

# -

# (d) Variance explained:

summary(model_1)

# --> neuroticism explains 35% of the variance in depression scores

# -

# (e) Model 2:

model_2 <- lm(ces_d ~ male + age + neuroticism)
summary(model_2)

# --> age and neuroticism are significant predictors of depression even after accounting for sex differences
# --> model 2 explains 36% of the variance in depression scores

# -

# (f) Comparing model fits:

anova(model_1, model_2)

# --> model 2 fits the data better than model 1

# --

# 2. Modelling: Extraversion

# -

# (a) Model 3:

model_3 <- lm(ces_d ~ male + age + neuroticism + extraversion)
summary(model_3)

# --> also extraversion is a significant predictor of depression
# --> model 3 explains 37% of the variance in depression scores

# -

# (b) Comparing model fits:

anova(model_2, model_3)

# --> model 3 fits the data better than model 2

# -

# (c) Model 4:

model_4 <- lm(ces_d ~ male + age + neuroticism + extraversion + male:neuroticism + male:extraversion)
summary(model_4)

# --> interaction between sex and neuroticism is significant, between sex and extraversion not
# --> model 4 explains 37% of the variance in depression scores

anova(model_3, model_4)

# --> model 4 fit the data better than model 3

# -

# (d) Interpreting model 4:

# There is a sex difference in the effect of Neuroticism on depression
# In males, the effect of Neuroticism on depression is smaller than in females

# But no sex difference in the effect of Extraversion on depression
# --> this interaction can be dropped out from future models

# --

# 3. Modelling: Health problems

# -

# (a) Model 5:

model_5 <- lm(ces_d ~ male + age + health_problems + neuroticism + extraversion + male:neuroticism)
summary(model_5)

# --> after accounting for health problems, effects of personality are still significant
# --> also number of health problems is a significant predictor of depression
# --> model 5 explains 39% of the variance in depression scores

# --> however, the link between personality and depression is still real (and not just a reflection of one's present health state)

# -

# (b) Model 6

model_6 <- lm(ces_d ~ male + age + health_problems)
summary(model_6)

# --> model 6 explains 7% of the variance in depression scores

# model 5 explains 37% of the variance, model 6 only 7% of the variance
# and it is less parsimonious --> model 5 fits the data better

# -

# (c) Conclusion:

# Both current health status and personality (including neuroticism and extraversion) significantly predict depression.

# -----
