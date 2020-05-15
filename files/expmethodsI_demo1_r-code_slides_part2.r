# Experimental and Statistical Methods in Biological Sciences I
# Demo 1, part II: Preparing your data for statistical analyses
# Heini Saarimäki, 18.9.2014

# -----

# The first step when starting with a new data set is to get familiar with what you have.
# You might need to prepare your data using these few steps:
	# Checking the data types in your data.
	# Checking for outliers and missing data.

# -----

# 1. Recap from part I:

# We have already encountered several types of data:

# numeric
num <- 1.2
num
num.v <- c(1.2, 2.2, 3.0)
chr.v

# character
chr <- 'cat'
chr
chr.v <- c('cat', 'dog', 'mouse')
chr.v

# We have seen simple operations on that data:
num.v/2

# We have seen data collected in data frames:
my.df <- data.frame(x=rnorm(10), y=1.10)

# And we have learned about subscripting and functions:
my.df[,2]
my.df[2,]
mean(my.df$x)

# But what if our data represents categories?
# Numerically related categories --> use a number
# Completely unrelated categories --> use a character
# Categories related but not numerically --> use a factor

# -----

# 2. Factors

# Load in some example data:
AgeWeight <- read.csv('')
head(AgeWeight)

# Notice that sex (coded as male=1) is an independent variable with a categorical response.

# To add a factor to your data frame, create a set of suitable labels:
my.df$iv <- rep(c('m', 'f'), 5)
my.df$if

# But my.df$iv is still just a set of characters. You need to tell R that it's a factor:
my.df$iv <- factor(rep(c('m', 'f'), 5))
my.df$iv

# Factors can also be represented by numbers.
# Compare the two variables below:
a.v <- rep(c(1:5), 2)
a.v
b.v <- factor(rep(c(1:5), 2))
b.v

# Note that when reading in .csv files, R assumes numbers are numbers and characters are factors.
# You have to tell R if numbers are factors.
# For example, examine AgeWeight dataset using the useful summary function:

summary(AgeWeight)

# Now, note how summary represents different types of variables:

x <- c(1:10)	# numerical
summary(x)
y <- c('cat', 'dog', 'mouse', 'horse')	# categorical
summary(y)
z <- factor(c('cat', 'dog', 'dog', 'horse'))	# factor
summary(z)

# Now, examine the summary of AgeWeight again:
summary(AgeWeight)

# Notice that male is not coded as a factor yet.

# Another way to examine whether, e.g., variables are coded as factors is by checking the object classes.
# Everything in R is actually an 'object' of a given class.
# summary and other functions take the class into account.

class(x)
class(y)
class(z)
class(class)	# !!

# When you type the name of an object, you are implicitly using the function print().
# print won't necessarily tell you everything that's 'in' the object.
# It will just tell you useful information.
# To really see what's 'in' the object, use str().

z
print(z)	# equivalents
str(z)

# When you use a function, such as mean, it returns an object:
mean(x)
print(mean(x))	# equivalents
class(mean(x))	
m <- mean(x)	# this stores the result
class(m)

# Complicated functions return complicated objects!
o.lm <- lm(x~y, my.df)	# stores your model for linear regression
summary(o.lm)		# results of the linear regression

# The 'R' way: create objects, then explore them:
cooks.distance(o.lm)	# more results of the linear regression
plot(o.lm, which=1:2)	# plotting these results

# -----

# 3. Outliers and missing data

# Always make sure your data makes sense.
# Sometimes you or somebody who coded the data made an error.
# Or if you use somebody else's data, the missing values might have been funnily coded.
# The effects of these anomalities have to be removed!

# The solution is to code the missing values/outliers as NA.
# This way you don't have to remove the whole observation,
# but you can remove the effects of missing or outlier values from your analyses.

# Explore your data numerically and graphically:
summary(AgeWeight)
library(psych)
describe(AgeWeight)
boxplot(AgeWeight$weight)

# R uses the special code NA to represent a missing value
my.df
my.df[7,2] <- NA
my.df
summary(my.df)

# Simple R functions are picky about data with missing values:
mean(my.df$x)
mean(my.df$x, na.rm=TRUE)

# -----
# 



