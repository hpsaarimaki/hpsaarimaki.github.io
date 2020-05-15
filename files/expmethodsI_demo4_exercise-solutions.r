# Demo 4, 9.10.2014
# Example solutions
# Heini Saarimäki

# -

# 1.2 Import data

setwd('Z:/Desktop/Demo4')

students <- read.table('http://becs.aalto.fi/~heikkih3/students', header=T, sep="")


# -

# Question 1

head(students)
summary(students)

# notice that Educ is not yet coded as a factor,
# and that Extra has a lot of missing values (NA's)
# and that Stress has funny values (999's): probably tried to code as missing values

# recode factor Educ as factor:

students$Educ <- factor(students$Educ)
dim(students)

# locate the outliers:

which(students$Stress == 999)
# rows 37, 40, 43, 46, 50

# -

# Question 2

# values 999 in variable Stress look like an attempt to code NAs

# recode these to NAs and save to a new data frame 'students2':

students2 <- students
students2$Stress[which(students2$Stress == 999)] <- NA

summary(students2)

# -

# Question 3

# The benefit is that you can compare different analysis strategies.

# For instance, you might argue that anyone with an "Hours" score of over 10
# has missed too much of the course to engage with it and properly apply
# themselves to the exam/rate the teacher (difficult if they have missed nearly 
# all the classes!), and exclude them from analysis.
 
# However, another researcher might disagree. You could save one version with
# the cases deleted to data2, and another version with them included to data3.

# Then you could run your analyses on data2 and data3 to see whether the removal
# has any effect. And the original dataset is never overwritten in this process.

# -

# attach the data frame at this point:

attach(students2)

# -

# Question 4

# plotting:

names(students2)

# histograms:

layout(matrix(c(1,2,3,4,5,6),3,2))
hist(Performance)
hist(Hours)
hist(Rating)
hist(Entry)
hist(Stress)
hist(Extra)

# boxplots:

layout(matrix(c(1,2,3,4,5,6),3,2))
boxplot(Performance, main="Performance")
boxplot(Hours, main="Hours")
boxplot(Rating, main="Rating")
boxplot(Entry, main="Entry")
boxplot(Stress, main="Stress")
boxplot(Extra, main="Extra")

# Scatterplots:

plot(students2)

# -

# Question 5

# There appear to be some trends:
# A weak negative association between "Performance" and "Hours" and
# a positive association between "Performance" and "Rating", and an
# even weaker positive association between "Performance" and "Stress".

# Note only the few observations in "Extra".

# ---

# 2. Correlations

# 2.1 Pearson product-moment correlation coefficient:

# take the correlation between two variables:

cor.test(Performance, Hours)	

# plot the relationship:

plot(Performance, Hours)

# take the whole correlation matrix for the data frame:

library('psych')		# install.packages('psych')

corr.test(students2[c(1:2,4:7)])	# include only the numeric variables

# -

# Question 6

# There are no correlations between:
# Extra and any other variable (!)
# Performance and Entry
# Entry and Rating
# Stress and Rating
# Entry and Stress

# -

# Question 7

# maximum sample size 228
# minimum sample size 37

# This matters because if we used the variable with just 37 cases ("Extra")
# we will be greatly limiting out power. If you look at the size of the
# correlations, "Extra" correlates at r=.31 with "Entry" yet is non-significant.
# The sample size for this variable isn't large enough to have confidence in
# the results.

# -

# Question 8

plot(students2)

# "Extra" is non-significant for the reasons given above: inspection of the 
# plots shows "Extra" has far fewer data points (only 37) than the other plots.

# -

# Question 9

# No. Note that "Hours" correlates negatively with ALL the other variables.

# ---

# 2.2 Spearman rank correlation coefficient:

# take correlation using rank correlation coefficient:

cor.test(Performance, Hours, method="spearman")

# -

# Question 10

# correlation matrix using rank correlation coefficient:

corr.test(students2[c(1:2, 4:7)], method="spearman")

# -

# Question 11

# save results of correlation using both product-moment and rank coefficients:

pearson <- corr.test(students2[c(1:2, 4:7)])
spearman <- corr.test(students2[c(1:2, 4:7)], method="spearman")

# observe the structure of the correlation matrix object:

str(pearson)

# take out just p value matrix:

pearson$p

# if you want to round things:

round(pearson$p, 2)
round(spearman$p, 2)

# Now you can compare the matrices above.
# Note that Pearson and Spearman give almost similar results.
# The biggest difference is that with Spearman, the correlation
# between "Performance" and "Stress" is not significant.
# if you want to look up the correlation coefficients:

round(pearson$r, 2)
round(spearman$r, 2)

# coefficients vary only slightly between the two methods

# ---

# 2.3 Partial correlations

install.packages('ppcor')
library(ppcor)
?pcor.test

pcor.test(Performance, Entry, Hours)

# -----

# 3. Regression

# ---

# 3.1 Running a basic model

# does attendance (Hours) predict performance in exam (Performance)?

model1 <- lm(Performance ~ Hours)

summary(model1)

# --> attendance predicts exam performance (b=-3.3, t(226)=-5.18, p<.001)
# --> attendance explain 10% of the variance in performance (F(1,226)=26.83, p<.001, adjusted R^2 = 0.10)

# ---

# 3.2 Evaluating assumptions

# Multicollinearity

# i.e. whether predictors correlate strongly with each other

corr.test(students2[c(1:2, 4:7)])

# --> no very strong correlations in this data set
# (all correlations < 0.4)

# -

# Normality

# look at the qq-plot, histogram or explicit tests for normality (see Demo 3)

# see Question 4 for histograms:
# all variables except for "Extra" seem to be normally distributed

# -

# Homoscedasticity

# i.e. are the residuals random?

# you can either look at the "Residuals vs Fitted" plot in regression plots
# (residuals should be equally distributed, i.e. no trends)

plot(model1)

# or test for homoscedasticity explicitly (test result should be non-significant):

install.packages('car')
library(car)
ncvTest(model1)

# -

# Influential cases

# i.e. look whether any cases unduly influence the model

# you can look at "Residuals vs Leverage" plot in regression plot: 
# shows Cook's distances, should be below 0.5 or 1
# the 0.5 and 1 levels are shown with dashed red line
# in this model, no highly influential cases (you cannot even see the dashed red line!)

# you can also test for influential cases separately:
# the column labeled "cook.d" shows values for Cook's distances,
# and these should be below 0.5

influence.measures(model1)

# ---

# 3.3 Multiple regression

# do attendance (Hours) and performance at the beginning of the course (Entry)
# predict performance in exam (Performance)?

model2 <- lm(Performance ~ Hours + Entry)

summary(model2)

# --> attendance predicts performance (b=-3.4, t(225)=-5.2, p<.001) 
# --> but entry performance does not 
# --> model fits data and explains 10% of variance in Performance (F(2,225)=13.7, p<.001), adjusted R^2=0.10)
# --> but is the fit better than in model1 (see Question 13)?

# -

# Question 12

# the new predictor Entry is not significant

# -

# Question 13

# compare models:

anova(model1,model2)

# adding Entry as a predictor does not improve the model fit significantly

# -

# Question 14

# we should not keep Entry, because adding it in model makes the model more
# parsimonious (complex) with no meaningful increase in fit

# -

# Question 15

# Extra has very few cases. As a result it will make any model that uses it
# as a predictor very low-powered and it is best ignored.

# see what happens:

model.x <- lm(Performance ~ Extra)
summary(model.x)

# the model is not significant (F(1,35)=1.13, p=0.296)

# -

# Question 16

# do attendance (Hours) and opinion about the course (Rating) predict 
# performance in exam (Performance)?

model3 <- lm(Performance ~ Hours + Rating)

# -

# Question 17

summary(model3)

# --> Rating is a significant predictor (b=1.7, t(225)=2.4, p<.05)

# -

# Question 18

# model fit in model 1 is 0.10, in model 3 0.12
# i.e. model 3 explains a bit more variance, but is it significantly more?

anova(model1, model3)

# --> model 3 fits data better than model 1 (p<.05)

# -

# Question 19

# you should examine Adjusted R-squared value in summary() output

# -

# Question 20

# a little over 12%

# ---

# 3.4 Interactions

# do attendance (Hours) and opinion (Rating) and their interaction
# predict performance in exam (Performance)?

model4 <- lm(Performance ~ Hours * Rating)

# -

# Question 21

# main effects

# - 

# Question 22

summary(model4)

# --> interaction is not significant 

# -

# Question 23

anova(model3,model4)

# --> adding interaction does not improve model fit

# ---

# 3.5 Nonlinear effects

# create a new variable in the data frame:

students2$Rating2 <- students2$Rating^2
attach(students2)
summary(students2)

# -

# Question 24

# quadratic function takes the shape of either u-shape or inverted u-shape

# -

# Question 25

# does Rating2 predict Performance?

model5 <- lm(Performance ~ Rating + Rating2)
summary(model5)

# --> Rating2 does not predict Performance
# therefore, the trend is not quadratic. a linear trend is the best explanation of the association.

# -

# Question 26

# let's try a full model!
# include all other variables except for Extra (it has a lot of missing values)

names(students2)

model6 <- lm(Performance ~ Hours * Educ * Rating * Entry * Stress)

summary(model6)

# --> none of the predictors is significant
# --> model is very complicated, but not significant

# multiple R^2 increases by a large margin (from 0.13 in model 3 to 0.27 in model 6)
# adjusted R^2 actually decreases (0.12 in model 3 to 0.08 in model 6)

# -

# Question 27

# adjusted R^2 penalizes for complexity. inclusion of "junk" predictors
# does not improve the quality of the model.
# multiple R^2 will not account for the need for parsimony, and will increase
# as further predictors are added.
# adjusted R^2 is the more useful value.

# -----

# 4. Contrasts

summary(students2)

# -

# Building up contrasts

levels(Educ)
contrasts(Educ) <- cbind(c(-0.5, -0.5, 1), c(1, -1, 0))

# -

# Running the comparison

attach(students2)

A1 <- aov(Performance ~ Educ)

summary(A1)

M1 <- lm(Performance ~ Educ)

summary(M1)

# -----

# 5. Exercises

naming <- read.csv("http://becs.aalto.fi/~heikkih3/naming_wide.csv", header=T)

# are factors coded as factors? are there extreme outliers?

summary(naming)

layout(matrix(c(1,2,3,4),2,2))
boxplot(naming$iq, main="iq")
boxplot(naming$hrs, main="hrs")
boxplot(naming$ms.regular, main="ms.regular")
boxplot(naming$ms.exception, main="ms.exception")

# --> factors are already coded as factors, no extreme outliers or missing values

attach(naming)

# -

# Question 28

plot(naming)
corr.test(naming[c(1:2,4:5)])

# -

# Question 29

# yes, sample size in all variables is 998

# -

# Question 30

# Significant correlations:
# strong negative correlation between iq and time spent reading regular words (r=-0.6, p<.001)
# negative correlation between iq and time spent reading exceptional words (r=-0.4, p<.001)
# weak negative correlation between reading hours and time spent reading regular words (r=-0.1, p<.001)
# negative correlation between reading hours and time spent reading exceptional words (r=-0.3, p<.001)
# also a positive correlation between time spent reading regular and exceptional words (r=0.3, p<.001)

# Verbal interpretation:
# higher IQ is related to faster reading times of both regular and exceptional words
# more reading hours is related to faster reading times especially in exceptional words
# reading times of both regular and exceptional words are related

# -

# Question 31

# does IQ predict reading times of regular words?

model.1 <- lm(ms.regular ~ iq)

# H0: IQ does not predict reading times of regular words
# H1: IQ predicts reading times of regular words

# -

# Question 32

summary(model.1)

# --> IQ predicts reading time of regular words (b=-3.0, t(996)=-23.7, p<.001)
# --> IQ explains 36% of the variance in reading times (F(1,996)=560.3, p<.001, adjusted R^2 = 0.36)

# -

# Question 33

# do IQ and reading hours predict reading times of regular words?

model.2 <- lm(ms.regular ~ iq + hrs)
summary(model.2)

# --> also reading hours predict reading times of regular words (b=-15, t(995)=-3.97, p<.001)
# --> model explains 37% of the variance in reading times (F(2,995)=292.2, p<.001, adjusted R^2 = 0.36)

# -

# Question 34

# which model fits the data better?

anova(model.1, model.2)

# --> model 2 fits the data better

# i.e., adding reading hours in the model significantly improves the fit
# we should keep both IQ and reading hours in the future models

# -

# Question 35

# does adding interaction in the model improve it?

model.3 <- lm(ms.regular ~ iq * hrs)
summary(model.3)

# --> interaction effect is not significant

# compare model 2 and 3:

anova(model.2,model.3)

# --> adding interaction effect does not improve the model

# -

# Question 36

# notice that in the exercise sheet the instruction told you to use effects coding;
# in fact, we did not go through this but instead used contrasts in the examples
# I added both solutions below:

# 1) you could solve this by using contrasts this way:

# check the order of the categories
levels(reading_time)			
# then add the contrast comparing high and low reading groups
contrasts(reading_time) <- c(1,-1,0)

# 2) or, you can create a new variable (here, ec_1) which codes effect (=comparison) between low and high reading groups

naming$ec_1 <- 0
naming$ec_1[which(reading_time=="low")] <- -1
naming$ec_1[which(reading_time=="high")] <- 1

head(naming)
attach(naming)

# -

# Question 37

# 1) using contrasts:

anova.1a <- aov(ms.regular ~ reading_time)
summary(anova.1a)
summary.lm(anova.1a)

# 2) or using effects coding:

anova.1b <- aov(ms.regular ~ ec_1)
summary(anova.1b)

boxplot(ms.regular ~ ec_1)

# --> low and high reading groups differ significantly

# -

# Question 38

# 1) using contrasts:

# check the order of the categories
levels(reading_time)			
# then add a contrast comparing high and low reading groups, and another contrast comparing high and medium groups
contrasts(reading_time) <- cbind(c(1,-1,0), c(1,0,-1))

# 2) or using effects coding:

naming$ec_2 <- 0
naming$ec_2[which(reading_time=="medium")] <- -1
naming$ec_2[which(reading_time=="high")] <- 1

attach(naming)

# -

# Question 39

# 1) using contrasts:

anova.2a <- aov(ms.regular ~ reading_time)
summary.lm(anova.2a)

# 2) or using effects coding:

anova.2b <- aov(ms.regular ~ ec_2)
summary(anova.2b)

# --> medium and high reading groups do not differ significantly
