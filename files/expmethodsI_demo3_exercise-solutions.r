# Demo 3: T tests and ANOVA
# Example script for handout
# Heini Saarimäki 2.10.2014

# -----

# 1. Data preparations and descriptive statistics

# ---

# 1.2 Load and prepare your data

setwd('Z:/Desktop/Demo3')

deprivation <- read.csv('http://becs.aalto.fi/~heikkih3/deprivation.csv')

# Look at the data:

head(deprivation)		# 10 variables
dim(deprivation)		# 47 rows (i.e. 47 participants, one per row)

summary(deprivation)	

# categorical variables ID, age and education are not yet coded as factors
# --> change ID, age and education to factors

deprivation$ID <- factor(deprivation$ID)
deprivation$age <- factor(deprivation$age)
deprivation$education <- factor(deprivation$education)

summary(deprivation)	# looks allright now

# look for missing values:

summary(deprivation)
boxplot(deprivation[5:7])	# take boxplot for digit symbol scores at all time points
boxplot(deprivation[8:10])	# similar boxplot for benton error scores

# no very extreme outliers (at least no wrongly coded missing values)
# so no missing values in the data, no need to correct these

# when you're happy with how the data looks like, use attach:

attach(deprivation)

# ---

# 1.3 Describe your data

# check frequencies for each group in all categorical variables:

summary(deprivation[2:4])

# you can also check for contingency tables:

table(age,education)
table(age, ht_group)
table(education, ht_group)

# table of descriptive statistics for continuous variables:

library('psych')			# load package 'psych' to get describe function
describe(deprivation[5:10])	# take descriptive statistics

# if you want to save the descriptive stats table for further use:

deprivation.descriptives <- describe(deprivation[5:10])
write.csv(deprivation.descriptives, "deprivation_descriptives.csv")

# plot continuous variables:

layout(matrix(c(1,2,3,4,5,6),3,2))		# create 6-cell display first
boxplot(digitsymbol_1, ylim=c(0,80), main="Digit symbol score, time 1")
boxplot(digitsymbol_2, ylim=c(0,80), main="Digit symbol score, time 2")
boxplot(digitsymbol_3, ylim=c(0,80), main="Digit symbol score, time 3")
boxplot(bentonerror_1, ylim=c(0,14), main="Benton error score, time 1")
boxplot(bentonerror_2, ylim=c(0,14), main="Benton error score, time 2")
boxplot(bentonerror_3, ylim=c(0,14), main="Benton error score, time 3")

# -----



# 2. Distributions

# 2.1 Normal distribution

# the original distribution of 10000 samples of random integers between 0 and 10 is uniform:

hist(runif(10000)*10, main="")  # here, runif function creates your simulated values

# calculate mean for 10000 samples of 5 integers between 0 and 10 using for-loop:

means <- numeric(10000)
for (i in 1:10000) {means[i] <- mean(runif(5)*10) }

# finally, see how these means are normally distributed:

hist(means, ylim=c(0,1600))

# let's add a normal curve:

h <- hist(means, freq=F, ylim=c(0,0.35))
x <- seq(min(h$breaks),max(h$breaks), by=0.1)
y <- dnorm(x, mean=mean(means), sd=sd(means))
lines(x,y,col="red", lty="dashed", lwd=3)

# ---

# 2.2 Student's t-distribution

# create a vector that includes simulated values between -4 and 4:

simu <- seq(-4,4,0.01)

# draw a normal distribution:

plot(simu, dnorm(simu),  type="l", lty=2, ylab="Probability density", xlab="Deviates")

# add t-distributions for different sample sizes 

lines(simu, dt(simu, df=5), col="red")		# sample size 5
lines(simu, dt(simu, df=10), col="green")		# sample size 10
lines(simu, dt(simu, df=20), col="yellow")	# sample size 20
lines(simu, dt(simu, df=30), col="blue")		# sample size 30
lines(simu, dt(simu, df=100), col="grey")		# sample size 100

# notice that when sample size increases to 30 and beyond, t-distribution resembles normal distribution

# ---

# 2.3 F-distribution

# create a vector that includes simulated values between 0 and 4

simu2 <- seq(0,4,0.01)

# draw an F-distribution with degrees of freedom 1 and 1:

plot(simu2, df(simu2, 1,1), type="l", lty=2, ylab="Probability density", xlab="Deviates")

# try changing each degrees of freedom:

lines(simu2, df(simu2, 1,10), col="red")		# df1 = 1, df2 = 10
lines(simu2, df(simu2, 1,40), col="magenta")	# df1 = 1, df2 = 40

lines(simu2, df(simu2, 2,40), col="blue")		# df1 = 2, df2 = 40
lines(simu2, df(simu2, 3,40), col="cyan")		# df1 = 3, df2 = 40
lines(simu2, df(simu2, 4,40), col="green")	# df1 = 4, df2 = 40

# -----

# 3. Tests for normality

# descriptive statistics:

describe(deprivation[5:10])		# look for mean, median, skewness
# for normality, mean should be close to median and skewness close to 0

# histograms:

layout(matrix(c(1,2,3,4,5,6), 3,2))			# set layout to 6-cell
hist(digitsymbol_1, main="Digit symbol 1")
hist(digitsymbol_2, main="Digit symbol 2")
hist(digitsymbol_3, main="Digit symbol 3")
hist(bentonerror_1, main="Benton error 1")
hist(bentonerror_2, main="Benton error 2")
hist(bentonerror_3, main="Benton error 3")

# note the skewness in Benton error scores

# use quantile-quantile plots (observed values = dots should be close to line)

qqnorm(digitsymbol_1)
qqline(digitsymbol_1)

# tests for normality:

# Kolmogorov-Smirnov for Digit symbol score time 1:
ks.test(digitsymbol_1, "pnorm", mean=mean(digitsymbol_1), sd=sd(digitsymbol_1))
# suggests normality

# Shapiro-Wilk:
shapiro.test(digitsymbol_1)
# suggests normality

# ---

# 3.1 Exercises

# Question 1

layout(matrix(c(1,2,3,4,5,6), 3,2))			# set layout to 6-cell
qqnorm(digitsymbol_1, main="Digit symbol 1")
qqline(digitsymbol_1)
qqnorm(digitsymbol_2, main="Digit symbol 2")
qqline(digitsymbol_2)
qqnorm(digitsymbol_3, main="Digit symbol 3")
qqline(digitsymbol_3)
qqnorm(bentonerror_1, main="Benton error 1")
qqline(bentonerror_1)
qqnorm(bentonerror_2, main="Benton error 2")
qqline(bentonerror_2)
qqnorm(bentonerror_3, main="Benton error 3")
qqline(bentonerror_3)

# all variables look relatively normally distributed

# -

# Question 2

# Digit symbol time point 1:
ks.test(digitsymbol_1, "pnorm", mean=mean(digitsymbol_1), sd=sd(digitsymbol_1))
shapiro.test(digitsymbol_1)
# --> normally distributed

# Digit symbol time point 2:
ks.test(digitsymbol_2, "pnorm", mean=mean(digitsymbol_2), sd=sd(digitsymbol_2))
shapiro.test(digitsymbol_2)
# --> normally distributed

# Digit symbol time point 3:
ks.test(digitsymbol_3, "pnorm", mean=mean(digitsymbol_3), sd=sd(digitsymbol_3))
shapiro.test(digitsymbol_3)
# --> normally distributed

# Benton error time point 1:
ks.test(bentonerror_1, "pnorm", mean=mean(bentonerror_1), sd=sd(bentonerror_1))
shapiro.test(bentonerror_1)
# --> normally distributed

# Benton error time point 2:
ks.test(bentonerror_2, "pnorm", mean=mean(bentonerror_2), sd=sd(bentonerror_2))
shapiro.test(bentonerror_2)
# --> KS suggests normality SW does not

# Benton error time point 3:
ks.test(bentonerror_3, "pnorm", mean=mean(bentonerror_3), sd=sd(bentonerror_3))
shapiro.test(bentonerror_3)
# --> KS suggests normality SW does not

# Benton error scores at times 2 and 3 might not be normally distributed.
# For t tests and ANOVA we assume normality, so this assumption might not
# be fulfilled in these two variables.
# You might want to consider non-parametric tests.

# However, here for practice purpose, we consider all variables normally distributed.

# -----

# 4 T tests

# --

# 4.1 One-sample t test

# compare sample mean to population mean:

summary(digitsymbol_1)		# sample mean 42.23, population mean 40

# is this difference statistically significant?
# use one-sample t-test:

t.test(digitsymbol_1, mu=40)

# --> mean of our sample does not differ from population mean (t(46)=1.33, p>.05)

# ---

# 4.2 Independent samples t-test

# take t test for age differences in Digit symbol score time 1:

t.test(digitsymbol_1 ~ age)	# categorical variable after ~ 

# --> mean in the two age groups does not differ (t(35)=0.27, p>.05)

# without correction for equality of variances:

t.test(digitsymbol_1 ~ age, var.equal=T)

# in this case the results are the same regardless of the equality variances assumption

# visualize results:

boxplot(digitsymbol_1 ~ age, xlab="Age group", ylab="Digit symbol score")

# ---

# 4.3 Repeated-measures t-test

# take t test for Digit score differences between time points 1 and 2:

t.test(digitsymbol_1, digitsymbol_2, paired=T)

# --> no differences between time points 1 and 2 (t(46)=-0.79, p>.05)

# visualize results:

boxplot(digitsymbol_1, digitsymbol_2, ylab="Digit symbol score", xlab="Time point")

# ---

# 4.4 Exercises

# Question 3

# (i) Differences in Digit symbol baseline performance between hormone treatment groups?

t.test(digitsymbol_1 ~ ht_group)	

# --> no differences (t(44)=-0.17, p>.05)

# (ii) Differences in Benton error baseline performance between hormone treatment groups?

t.test(bentonerror_1 ~ ht_group) 

# --> control group has higher number of errors (t(45)=3.01, p<.01)

# use boxplot to visualize this result:

boxplot(bentonerror_1 ~ ht_group, xlab="Hormone treatment", ylab="Benton error score")

# -

# Question 4

# (i) Differences in Digit symbol scores between times 1 and 3

t.test(digitsymbol_1, digitsymbol_3, paired=T)
boxplot(digitsymbol_1, digitsymbol_3, xlab="Time point", ylab="Digit symbol score")

# --> test scores are higher at time 3 (t(46)=-6.94, p<.001)
# interpretation: test scores improve due to learning? (from 1st to 3rd measurement)

# (ii) Differences in Digit symbol scores between times 2 and 3

t.test(digitsymbol_2, digitsymbol_3, paired=T)
boxplot(digitsymbol_2, digitsymbol_3, xlab="Time point", ylab="Digit symbol score")

# --> test scores are higher at time 3 (t(46)=-10.60, p<.001)
# interpretation: test scores improve after a night of normal sleep

# -

# Question 5

# (i) Differences in Benton error scores between times 1 and 2

t.test(bentonerror_1, bentonerror_2, paired=T)

# --> no differences (t(46)=1.53, p>.05)
# interpretation: no effect of sleep deprivation on error rate (maybe learning effect cancels this effect?)

# (ii) Differences in Benton error scores between times 1 and 3

t.test(bentonerror_1, bentonerror_3, paired=T)
boxplot(bentonerror_1,bentonerror_3, xlab="Time point", ylab="Benton error score")

# --> error rates are lower at time 3 (t(46)=5.83, p<.001)
# interpretation: error rates improve due to learning? (from 1st to 3rd measurement)

# (iii) Differences in Benton error scores between times 2 and 3

t.test(bentonerror_2, bentonerror_3, paired=T)
boxplot(bentonerror_2, bentonerror_3, xlab="Time point", ylab="Benton error score")

# --> error rates are lower at time 3 (t(46)=5.99, p<.001)
# interpretation: test scores improve after a night of normal sleep


# -----

# 5. Analysis of variance

# --

# 5.1 One-way ANOVA

# Does baseline performance in Digit symbol task vary between education groups?

A1 <- aov(digitsymbol_1 ~ education)
summary(A1)

# --> no differences between groups (F(2,44)=2.16, p>.05)

model.tables(A1, "means")	# mean score for each group

# visualize results:

boxplot(digitsymbol_1 ~ education, xlab = "Education group", ylab="Digit symbol score",	main="Baseline Digit symbol score by education group", col="grey")

# get diagnostic plots:

layout(matrix(c(1,2,3,4),2,2))
plot(A1)

# --

# 5.1.1 ANOVA assumptions

# test for homogeneity of variances:

install.packages('car')
library('car')
leveneTest(digitsymbol_1 ~ education)
bartlett.test(digitsymbol_1 ~ education)

# variances are equal in Digit symbol task at time point 1

# --

# 5.1.2 Visualizing results

# familiar box plot:

boxplot(digitsymbol_1 ~ education)

# means with confidence intervals:

install.packages('gplots')
library('gplots')
plotmeans(digitsymbol_1 ~ education, ylim=c(10,70), n.label=FALSE)

# ---

# 5.2 Two-way between subject ANOVA

# Does Digit symbol baseline performance differ between age and hormone treatment groups?

A2 <- aov(digitsymbol_1 ~ age*ht_group)
summary(A2)
model.tables(A2, "means")
layout(matrix(c(1,2,3,4),2,2))
plot(A2)

# --> no age main effect (F(1,43)=0.08, p>.05)
# --> no hormone treatment main effect (F(1,43)=0.89, p>.05)
# --> age and hormone treatment have a significant interaction effect (F(1,43)=6.81, p<.05)

# visualize results:

boxplot(digitsymbol_1 ~ age*ht_group, col=c("green", "grey"))	# not very clear..

interaction.plot(age, ht_group, digitsymbol_1, type="b", col=c(1:3),
	leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
	xlab="Age group", ylab="Digit symbol score", main="Interaction plot")

# --

# 5.2.1 Type of Sums of Squares

library(car)
Anova(A2, type=3)			# ANOVA with type 3 sums of squares

# --

# 5.2.2 Post-hoc comparisons

# Test where are the differences you found in ANOVA between age and hormone treatment effects:

pairwise.t.test(digitsymbol_1, education, p.adj="none")
pairwise.t.test(digitsymbol_1, education, p.adj="bonf")	# Bonferroni correction	
pairwise.t.test(digitsymbol_1, education, p.adj="holm")	# Holm correction

TukeyHSD(A1)								# Tukey's test

# ---

# 5.3 Exercises

# Question 6

# Does hormone treatment affect Digit symbol baseline differently in different education groups?

anova1 <- aov(digitsymbol_1~ht_group*education)
summary(anova1)

# --> no it does not, because interaction effect is nonsignificant (F(2,41)=1.24, p>.05)
# --> education and hormone treatment do not affect Digit symbol baseline performance (non-significant main effects)

# -

# Question 7

# Do education and age together affect the Digit symbol baseline performance?

anova2 <- aov(digitsymbol_1 ~ age*education)
summary(anova2)

# --> no they don't (only a trend towards significance in interaction effect) (F(2,41)=2.73, p>.05)
# --> age and education do not affect Digit symbol baseline performance (non-significant main effects)

# -

# Question 8

# Age, education and hormone treatment effects in Benton task baseline performance?

anova3 <- aov(bentonerror_1 ~ age*education*ht_group)
summary(anova3)

# --> only hormone treatment has a main effect on Benton task score (F(1,35)=10.21, p<.01)
# --> age and education do not have main effects
# --> age and education have an interaction effect (F(2,35)=3.66, p<.05)
# --> education and hormone treatment group have an interaction effect (F(2,35)=3.46, p>.05)

# visualize results:

boxplot(bentonerror_1 ~ ht_group, xlab="Hormone treatment group", ylab="Benton errors")
# --> hormone treatment group makes less errors than control group at baseline

interaction.plot(education,age,bentonerror_1)
# --> higher education improves scores (=less errors) in younger group, opposite trend in older group (!?)

interaction.plot(education,ht_group, bentonerror_1)
# --> higher education improves scores in hormone treatment group, slightly opposite in control group

# -----

# 6. Exercises

# Load in data:
naming <- read.csv('http://becs.aalto.fi/~heikkih3/naming_wide.csv')

# Get familiar with the data:
head(naming)
summary(naming)
dim(naming)

# It seems that the factors (sex, reading_time) are already coded as factors.
# No missing values (actually NA's from previous weeks have been removed totally!)
# Note that now 'ms' variable is split to 'ms.regular' and 'ms.regular
# - this explains why data has only half the rows from previous weeks:
# now one participant has all the variables in a single row.

detach()			# detach all previous data frames
attach(naming)		# attach naming data frame

# -

# Question 9

t.test(iq, mu=100)

# --> intelligence does not differ from the population mean (t(997)=0, p>.05).

# -

# Question 10

t.test(iq ~ sex)

# --> no sex differences in intelligence (t(961)=1.07, p>.05).

# -

# Question 11

t.test(hrs ~ sex)

# --> no sex differences in hours spent on reading (t(972)=0.29, p>.05).

# -

# Question 12

anova.1 <- aov(iq ~ reading_time)
summary(anova.1)

# --> no differences between reading categories in intelligence (F(2,995)=0.03, p>.05).

# -

# Question 13

t.test(ms.regular, ms.exception, paired=T)
mean(ms.regular)
mean(ms.exception)
boxplot(ms.regular, ms.exception, xlab="Word type (1=regular, 2=exceptional))", ylab="Time (ms)")

# --> reading times are shorter for regular words (t(997)=-68.8, p<.001).

# -

# Question 14

# Regular words: 

anova.2 <- aov(ms.regular ~ reading_time)
summary(anova.2)
boxplot(ms.regular ~ reading_time)	

# take a closer look at the differences using post-hoc comparison:
pairwise.t.test(ms.regular, reading_time, p.adj="bonf")
tapply(ms.regular, reading_time, mean)
tapply(ms.regular, reading_time, sd)

# --> there are differences between reading categories (based on hours spent on reading)
# in time taken to read regular words (F(2,995)=3.52, p<.05).
# --> post-hoc comparisons using a Bonferroni corrected pairwise t-test
# revealed that the differences are between the highest reading category (mean=993, sd=70.39)
# and the lowest one (mean=1009, sd=79).


# Exceptional words:

anova.3 <- aov(ms.exception ~ reading_time)
summary(anova.3)
boxplot(ms.exception ~ reading_time)

# take a closer look at the differences using post-hoc comparison:
pairwise.t.test(ms.exception, reading_time, p.adj="bonf")
tapply(ms.exception, reading_time, mean)
tapply(ms.exception, reading_time, sd)

# --> there are differences between reading categories (based on hours spent on reading)
# in time taken to read exceptional words (F(2,995)=38.99, p<.001).
# --> post-hoc comparisons using a Bonferroni corrected pairwise t-test
# revealed that all groups differ from each other:
# highest group (mean=1173, sd=76) has higher scores than the two other groups,
# and middle group (mean=1203, sd=79) has higher scores than the lowest group (mean=1228, sd=76).

# -

# Question 15

# Regular words:

anova.4 <- aov(ms.regular ~ sex * reading_time) 
summary(anova.4)

# interaction plot:
interaction.plot(reading_time, sex, ms.regular)

# --> sex does not modify the effect of reading time in 
# time taken to read regular words


# Exceptional words:

anova.5 <- aov(ms.exception ~ sex * reading_time) 
summary(anova.5)

# interaction plot:
interaction.plot(reading_time, sex, ms.exception)

t.test(ms.exception ~ sex)
boxplot(ms.exception ~ sex)

# neither does sex modify the effect of reading time in
# time taken to read exceptional words

# -----

