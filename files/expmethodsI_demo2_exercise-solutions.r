# Experimental and Statistical Methods in Biological Sciences I
# Demo 2: Descriptive statistics and plotting the data

# Heini Saarimäki 25.9.2014

# -----

# 1. Load in data

setwd('Z:/Desktop/Demo2')
ageweight <- read.csv("http://becs.aalto.fi/~heikkih3/age_weight2.csv")

# Examine your data

head(ageweight)
summary(ageweight)

# Save the original data into a new dataframe

ageweight.orig <- ageweight

# Change MALE into a factor

ageweight$MALE <- factor(ageweight$MALE)

# Change weird (< 0 ) WEIGHT values to NA

which(ageweight$WEIGHT <= 0)
ageweight[which(ageweight$WEIGHT <= 0), "WEIGHT"] <- NA

summary(ageweight)

# Attach the ageweight data

attach(ageweight)

# -----

# 2. Basics in plotting

# --

# 2.1 Basic structure of plotting functions


# Using plot function...

# For a numeric variable: scatterplot
plot(WEIGHT)

# For a factor (i.e., categorical variable): bar chart
plot(MALE)

# For 2 numeric variables: scatterplot
plot(AGE, WEIGHT)
plot(WEIGHT~AGE)		# equivalents!

# For a factor and a numeric variable: box plot
plot(MALE, WEIGHT)

# For 2 factors: bar chart
plot(MALE, SMOKE1)


# Other plotting functions..

# Histograms:
hist(WEIGHT)

# Box plots:
boxplot(WEIGHT)

# --

# 2.2 Modifying your plots

# Build up a nice scatterplot by adding named arguments:

plot(WEIGHT~AGE, main="Weight by Age",		# add main title
	xlab="Age (yrs)", ylab="Weight (kg)",	# add axis labels
	pch=16, col="blue",				# modify points
	xlim=c(0,70), ylim=c(0,140))			# scale axes

abline(lm(WEIGHT~AGE),					# draw regression line
	lty="dashed",					# modify line
	lwd=2,
	col="red")

# Make a nice histogram:

hist(WEIGHT, main="Weight", xlab="Weight", ylab="")

# Set total area size of histogram to 1:

hist(WEIGHT, main="Weight", xlab="Weight", ylab="", freq=F)

# Change the bin size

hist(WEIGHT)			# automatic binning

hist(WEIGHT, breaks=15)		# suggest the number of bins

bins=seq(20,140,by=20)		# suggest the size of a bin
hist(WEIGHT, breaks=bins)


# Advanced graphics:

# Sometimes you want to add a normal curve approximation to the plot.

# First, create the histogram object:
h <- hist(WEIGHT, freq=F, main="Distribution of Weights", 
	xlab="Weight (kg)", ylab="")

# Second, find highest and lowest values and create a sequence of them:
x <- min(h$breaks):max(h$breaks)

# Third, create vector y from normal distribution with same mean and sd as WEIGHT:
y <- dnorm(x, 
	mean=mean(WEIGHT, na.rm=T), 
	sd=sd(WEIGHT, na.rm=T))

# Finally, add the normal curve: 
lines(x,y,col="red", lty="dashed", lwd=3)

# --

# 2.3 Opening several plots in the same display

# E.g., put 4 plots in the same display:

layout(matrix(c(1,2,3,4),2,2))
hist(WEIGHT, main="Weight")
hist(HEIGHT, main="Height")
plot(SMOKE1, main="Smoking at time point 1")
plot(SMOKE2, main="Smoking at time point 2")

# --

# 2.4 Saving your plots

# First, open a device, then draw your plot, then close the device:

pdf(file="hist_weight.pdf")
hist(WEIGHT, main="Weight", xlab="Weight (kg)", ylab="")
dev.off()

# --

# 2.5 Exercises

# Question 1:

hist(WEIGHT, 
	xlim=c(0,150), 			# scales x axis
	breaks=6, 				# sets number of bins
	col="red", 				# sets bar colour
	xlab="Weight (kg)", 		# sets x label
	bg="grey")				# OUGHT TO set background colour?

# Note: bg looks like it should set the background colour,
# BUT it does not actually work this way (see the arguments 'hist' approves of by
# checking ?hist.
# If you want to do it, you should use this instead:

par(bg="grey")
hist(WEIGHT, xlim=c(0,150), breaks=6, col="red", xlab="Weight (kg)")

# Here, we define the parameter background with function 'par'
# The background will stay grey until we change it, so run this next:
par(bg="white")

# NOTE: If you want to change the 'par' arguments, the defaults are somewhat tricky
# to restore. If you are using 'par' arguments, it is good to save the defaults first
# in an object:
opar <- par()   # stores whatever parameters you have currently
#  Now, you can change what ever you want:
par(bg="green")
plot(WEIGHT, AGE)  # see what changed
# And restore the default values this way:
par(opar)


# -

# Question 2:

hist(WEIGHT, 
	xlim=c(0,150), 			
	breaks=6, 				
	col="red", 				
	xlab="Weight (kg)",
	main="")				# sets title to none

# -

# Question 3:
 		
# Hmm.. Maybe weight increases with age?

plot(AGE, WEIGHT)

# So it seems.

# -

# Question 4:

plot(AGE, WEIGHT,
	pch=10,				# set plot character type
	col="green")			# set plot character colour

abline(lm(WEIGHT~AGE))			# add regression line


# -

# Question 5:

plot(MALE, WEIGHT)

# Weight seems to depend on gender too.

# -

# Question 6:

# In question 3 both variables are numeric, so a scatterplot is produced.
# In question 5 there is one factor and one numeric variable, so a boxplot is produced.
# Output of plot function depends on input!

# -

# Question 7:

# An example plot:
plot(AGE, WEIGHT, pch=20, 
	main="Weight by Age", xlab="Age (yrs)", ylab="Weight (kg)")

# Changing font size larger for main title:

plot(AGE, WEIGHT, pch=20, 
	main="Weight by Age", xlab="Age (yrs)", ylab="Weight (kg)",
	cex.main=2)

# Changing font style to bold-italic for axes labels:

plot(AGE, WEIGHT, pch=20, 
	main="Weight by Age", xlab="Age (yrs)", ylab="Weight (kg)",
	font.lab=4)

# Changing font type for the whole graph:

plot(AGE, WEIGHT, pch=20, 
	main="Weight by Age", xlab="Age (yrs)", ylab="Weight (kg)",
	family="mono")


# -----

# 3. Descriptive statistics & plotting of categorical variables

# -

# 3.1 Contingency tables


# One categorical variable (same information as with summary...)

table(MALE)

# Two categorical variables:

table(MALE, SMOKE1)

# Or even more... 

table(MALE, SMOKE1, SMOKE2)

# Express frequencies as percentages:

round(prop.table(table(MALE, SMOKE1), margin=1)*100, 1)

# -

# 3.2 Plotting

# A simple bar chart:

plot(MALE)

# A boosted bar chart:

plot(MALE, col=c("red", "blue"), xlab="Gender")

# A simple bar chart for displaying two categorical variables:

plot(MALE, SMOKE1)

# Again then same one boosted:

plot(SMOKE1, MALE, col=c("red", "blue"),
	xlab="Smoking", ylab="Gender")

# -

# 3.3 Exercises


# Question 8:

table(SMOKE1, SMOKE2)

# 26 people quit smoking (smoked at time 1 but not at time 2)

# -

# Question 9

plot(SMOKE1, SMOKE2, 
	main="Smoking",
	xlab="Smoking, time 1", ylab="Smoking, time 2",
	col=c("green", "red"))

# Example: how to add frequency values to the plot:

smoke.table <- table(SMOKE1,SMOKE2)		# save contingency table

text(0.25,0.2,smoke.table[1,1], cex=2)
text(0.25,0.8,smoke.table[1,2], cex=2)
text(0.77,0.2,smoke.table[2,1], cex=2)
text(0.77,0.8,smoke.table[2,2], cex=2)


# -----

# 4. Descriptive statistics & plotting of continuous variables

# --

# 4.1 Descriptive statistics

# Summary:

summary(ageweight)

# Use describe function from 'psych' package:

install.packages('psych')		# install if necessary
library('psych')				# load for the use of this session
describe(ageweight)

# You can also take each descriptive statistic separately:

mean(WEIGHT, na.rm=T)			# na.rm=T removes missing values
var(WEIGHT, na.rm=T)

# Take average weight for men and women separately:

tapply(WEIGHT, MALE, mean, na.rm=T)

# Take average weight for female and male non-smokers and smokers separately:

tapply(WEIGHT, list(MALE, SMOKE1), mean, na.rm=T)

# Save your table in .csv format:

my.table <- tapply(WEIGHT, list(MALE, SMOKE1), mean, na.rm=T)
write.csv(my.table, "my_table.csv")

# --

# 4.2 Plotting

# Box plot of a numeric variable:

boxplot(WEIGHT)		# remember to modify boxplots too! (add labels etc.)

# Use box plot

layout(matrix(c(1,2)))
boxplot(ageweight$WEIGHT, main="Missing values removed")
boxplot(ageweight.orig$WEIGHT, main="Original data")

# Box plots can be taken for categories separately:

plot(MALE, WEIGHT, main="Weight by Gender", ylab="Weight (kg)")

# Histogram shows the shape of the distribution efficiently:

hist(WEIGHT)
hist(WEIGHT, breaks=20)		# adjust e.g. number of bins


# --

# 4.3 Exercises


# Question 10

mean(HEIGHT)
sd(HEIGHT)

# 

# -

# Question 11

height_mean <- tapply(HEIGHT, MALE, mean, na.rm=T)	# save table of means
height_sd <- tapply(HEIGHT, MALE, sd, na.rm=T)		# save table of sd's

height_table <- rbind(height_mean, height_sd)		# combine tables
height_table

# (a) women: mean height 1.66 (sd 0.07)
# (b) men: mean height 1.80 (sd 0.07)

# Save final table:

write.csv(height_table, "Height_by_gender.csv")

# -

# Question 12

hist(HEIGHT)

# -

# Question 13

h <- hist(HEIGHT, freq=F)
x <- seq(min(h$breaks),max(h$breaks), by=0.025)
y <- dnorm(x, mean=mean(HEIGHT, na.rm=T), sd=sd(HEIGHT, na.rm=T))
lines(x,y,col="red", lty="dashed", lwd=2)

# -

# Question 14

h <- hist(HEIGHT, freq=F, 
	ylim=c(0,5), 					# change y axis
	main="Distribution of Height", 		# add main title
	xlab="Height (m)", ylab="",			# add axis labels
	col="grey")						# change colour						
x <- seq(min(h$breaks),max(h$breaks), by=0.025)
y <- dnorm(x, mean=mean(HEIGHT, na.rm=T), sd=sd(HEIGHT, na.rm=T))
lines(x,y,col="red", lty="dashed", lwd=2)

# -

# Question 15

pdf(file="Height_distribution.pdf")
h <- hist(HEIGHT, freq=F, 
	ylim=c(0,5), 					
	main="Distribution of Height", 		
	xlab="Height (m)", ylab="",			
	col="grey")											
x <- seq(min(h$breaks),max(h$breaks), by=0.025)
y <- dnorm(x, mean=mean(HEIGHT, na.rm=T), sd=sd(HEIGHT, na.rm=T))
lines(x,y,col="red", lty="dashed", lwd=2)
dev.off()

# -

# Question 16

boxplot(HEIGHT)

# (a) median is a bit over 1.7 m
# (b) distribution looks quite normal, maybe a bit more higher values than lower

# -

# Question 17

boxplot(HEIGHT~MALE, main="Height by Gender", ylab="Height (m)", xlab="Gender")

# men seem to be taller than women

# -

# Question 18

layout(matrix(c(1,2)))
boxplot(WEIGHT, main="Box plot of Weight", ylab="Weight (kg)", col="grey")
hist(WEIGHT, main="Histogram of Weight", xlab="Weight (kg)", ylab="", col="grey")


# -----

# 5. Exercises


# Load in naming data

naming <- read.csv("http://becs.aalto.fi/~heikkih3/naming_new.csv")

# Check for missing values and that factors are factors

summary(naming)

# Everything looks ok!
# We have 2 factors (word.type, sex) already coded as factors
# ... and 3 numeric variables (iq, hrs, ms)
# Missing values in iq already coded as NA's
# Other values look fine too.

# Detach ageweight and attach naming:
detach(ageweight)
attach(naming)

# -

# Question 20

plot(word.type,ms)

# Reading times for regular words seem to be faster than for exceptional words.

layout(matrix(c(1,2)))
plot(word.type, ms, data=naming[which(sex=="female"),],
	main="Female")
plot(word.type, ms, data=naming[which(sex=="male"),],
	main="Male")

# Effect of word type on reading types seems similar in men and women.
# I.e., no gender effects on differences in reading times between different word types.

# -
 
# Question 21

# Take a subset of the data:

naming_subset <- naming[1:1000, c("iq", "hrs", "sex")]

# Check this new subset:

head(naming_subset)
summary(naming_subset)

# Detach whole data and attach just the subset:

detach(naming)
attach(naming_subset)

# - 

# Question 22

# Create factor reading group:

naming_subset$reading.group <- 'low'
naming_subset$reading.group[which(hrs>3.5)] <- 'medium'
naming_subset$reading.group[which(hrs>4.5)] <- 'high'
naming_subset$reading.group <- factor(naming_subset$reading.group)

# Check out the data now:

head(naming_subset)
summary(naming_subset)

# Need to attach the data again (cause we added a variable!)
# Previously added variables will be masked automatically

attach(naming_subset)

# -

# Question 23

table(sex,reading.group)		# contingency table
plot(table(sex,reading.group))	# bar charts

# No differences between men and women in how many participants are in which reading group.

# -

# Question 24

# Plot reading hours by gender:

boxplot(hrs~sex, main="Reading hours by gender", ylab="Reading hours")

# Shows similar lack of differences as question 23.

# -

# Question 25

# Plot intelligence by gender:

boxplot(iq~sex, main="Intelligence by gender", ylab="IQ")

# No differences in intelligence between men and women.

# -

# Question 26

# Plot histogram of intelligence with a normal curve approximation:

h <- hist(iq, freq=F, main="Distribution of IQ", xlab="IQ", ylab="", col="grey")
x <- min(h$breaks):max(h$breaks)
y <- dnorm(x, mean=mean(iq, na.rm=T), sd=sd(iq, na.rm=T))
lines(x,y,col="red", lty="dashed", lwd=2)

# Other ways to check for normality (see Section 6):
qqnorm(iq)					# QQ-plot
qqline(iq, col="red", lwd=2)
shapiro.test(iq)

# Both histogram, qq-plot and Shapiro-Wilk test for normality suggest the same:
# intelligence in this data set follows normal distribution.

# -

# Question 27

# Descriptive statistics for experimental manipulation (reading times by word type):

mean <- tapply(naming$ms, naming$word.type, mean, na.rm=T)
sd <- tapply(naming$ms, naming$word.type, sd, na.rm=T)

reading_times <- rbind(mean, sd)
reading_times

write.csv(reading_times, "reading_times.csv")


# Descriptive statistics for background variables: 

bg_variables <- describe(naming_subset)
bg_variables

write.csv(bg_variables, "background_variables.csv")


# -----

# 6. Basic tools for testing for normality

# 6.1 Histogram

# Resets the seed for randomization:

set.seed(111)

# Take a sample of 20 from normal distribution:

data_normal <- rnorm(20)

# And a sample of 20 from exponential distribution:

data_exp <- rexp(20)

# Look at the histograms:

layout(matrix(c(1,2), nrow=1))
hist(data_normal)
hist(data_exp)

# -

# 6.2 Q-Q plot

?qqnorm
layout(matrix(c(1,2), nrow=1))
qqnorm(data_normal)
qqline(data_normal)
qqnorm(data_exp)
qqline(data_exp)

# data_normal is relatively normally distributed,
# but data_exp is clearly not

# -

# 6.3 Statistical tests for normality

# Shapiro-Wilk test:

shapiro.test(data_normal)
shapiro.test(data_exp)

# Kolmogorov-Smirnov test:
ks.test(data_normal, "pnorm", mean=mean(data_normal), sd=sd(data_normal))
ks.test(data_exp, "pnorm", mean=mean(data_exp), sd=sd(data_exp))

# Normality tests confirm the observation from the Q-Q plots:
# data_normal is normally distributed, data_exp is not.

# ------

