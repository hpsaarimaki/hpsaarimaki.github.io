# Demo 1, Part I
# Basics of R
# Solutions to exercises
# Heini Saarimäki, 18.9.2014

# -----

# 1. Very basics

# Question 1
10+1
# 11

# Question 2
10-1
10*2
5/2
# these work as expected

# Question 3
10+1/2	# 10.5
(10+1)/2	# 5.5
# arithmetic operations performed from left to right, * and / first, then + - 
# change this order with brackets

# Question 4
x <- 1
x
# output is the value assigned to the variable

# Question 5
y <- (20/5)+100
z <- x+y
z		# 105

# Question 6
100 -> a
a
# direction of the arrow matters
# does not apply with = :
100 = a	# error!

# -----

# 2. Data types

# Question 7
c(10,20,30)+1
# results in a vector with values 11 21 31

# Question 8
my.vector <- c(10,20,30)+1
my.vector

# Question 9
40:100

# -----

# 3. Data structures

# Question 10
vector1 <- seq(1,20,by=0.5)
vector1
vector2 <- seq(1,20,length=5)
vector2
# adding the named argument "by" creates a sequence with steps increasing based on the value given
# adding "length" creates a sequence with the length based on the value given

# Question 11
rep(1:5, times=2)
rep(1:5, times=c(3,1,1,5,2))
rep(1:5, each=3)
#argument "times" with scalar input repeats the sequence 1:5
#argument "times" with vector input defines how many times each element of the sequence 1:5 is repeated
#argument "each" repeats each element of the sequence 

# Question 12
"a":"b"			# error! : cannot be used with characters
c("a", "b")			# c works
seq("a", "b", by=10)	# error! seq cannot be used
rep(c("a","b"), each=3)	# rep works

# Question 13
m <- 1:10
n <- seq(2,20, by=2)
matrix(m, nrow=2, ncol=5)
matrix(m, nrow=2, ncol=5, byrow=TRUE)
cbind(m,n)
rbind(m,n)
# "matrix" creates a matrix from a given set of values, columnwise by default
# "cbind" and "rbind" bind vectors together as columns and rows, respectively

# Question 14
AgeWeight <- read.csv("http://becs.aalto.fi/~heikkih3/age_weight.csv")
head(AgeWeight) 
# MALE is coded as 0's and 1's

# Question 15
head(AgeWeight)
# 59 years

# Question 16
tail(AgeWeight)
# tail shows the last rows of the data

# Question 17
?head
head(AgeWeight, 10)

# Question 18
dim(AgeWeight)	# 100, 6
ncol(AgeWeight)	# 6
nrow(AgeWeight)	# 100
# dim returns rows first, then columns

# Question 19
AgeWeight$AGE
class(AgeWeight$AGE)
# age is a numeric variable (integer)

# Question 20
?tolower
tolower("CAR")						# example
ageweight <- AgeWeight					# copy data to a new data frame
names(ageweight) <- tolower(names(AgeWeight)) 	# change variable names to lower case 
names(ageweight)						# check the results

# Question 21
summary(AgeWeight)

# -----

# Question 22
m <- c(48,32,78,22,16,60)
m[2:5]
m[c(2,3,4,5)]

# Question 23
m[length(m)-1]

# Question 24
AgeWeight[c(2,10,21),]

# Question 25
AgeWeight[c(1,2,30:45),]

# Question 26
AgeWeight[,c(3:4)]

# Question 27
AgeWeight[54,3]

# Question 28
AgeWeight[, c("WEIGHT", "AGE")]

# Question 29
AgeWeight[c(91:100), c("MALE", "AGE")]

# Question 30
100 + 35/(23*3) > 110 - 16/5.5
# FALSE

# Question 31
which(AgeWeight["AGE"] > 60)		# list the participants
AgeWeight[AgeWeight$AGE > 60,]	# separate the rows for these participants

# Question 32
AgeWeight[10,7]		# impossible, only 6 columns!
AgeWeight[10,6]		# access the 10th row in the 6th column instead
AgeWeight[4:5,2:3]	# rows 4:5, columns 2:3
AgeWeight[4:5,]		# rows 4:5, all columns
AgeWeight[c(4,6),c(1,3)]# rows 4 and 6, columns 1 and 3
AgeWeight[-(4:5),1]	# all rows except for 4 and 5, column 1
	
# Question 33
names(AgeWeight)				# cannot access 7th column
AgeWeight[10,"SMOKE2"]			# access the 10th row, 6th column instead
AgeWeight[4:5, c("AGE", "WEIGHT")]	# rows 4:5, columns 2:3
AgeWeight[c(4,6), c("MALE", "WEIGHT")] # rows 4 and 6, columns 1 and 3

# Question 34
AgeWeight[AgeWeight$MALE == 1,]		# test it works
age_weight.male <- AgeWeight[AgeWeight$MALE == 1,]	# stores the results in a new data frame
summary(age_weight.male)		# check the new data frame


# -----

# Question 35
x <- c(0,2,4,6,8,10)
y <- c(1,3,5,7,9,11)
x+y

# Question 36
AgeWeight$BMI <- (AgeWeight$WEIGHT)/((AgeWeight$HEIGHT)^2)
head(AgeWeight)

# Question 37
sum(x)			# test by using the newly created x (above)
sum(AgeWeight$WEIGHT)	# or using a variable
# the result is a sum of all the elements in the vector (or variable)

# Question 38
# lots of them: c, rep, seq, matrix, head, tail, summary, names, class, dim, ...
ls()

# Question 39
?sort
# sorts the elements of a vector in ascending or descending order

# Question 40
# packages are collections of ready-made functions
# usage of packages is two-folded:
# 1. you need to install the package
install.package('psych')
# 2. you need to load the package for the use of your current R session
library('psych')

# Question 41
range.size <- function(X) {
	F = max(X) - min(X)
	return(F) }

# Question 42
AgeWeight$SMOKE1 == "yes"
AgeWeight[which(AgeWeight$SMOKE1=="no" & AgeWeight$MALE == 1),]
AgeWeight[, "WEIGHT"]
range.size(AgeWeight[which(AgeWeight$SMOKE1=="no" & AgeWeight$MALE==1), "WEIGHT"])
# answer: range is 126.19 kg

# Question 43
range.size(c(1,5,10)) == 9			# TRUE
range.size(-234:119) == 353			# TRUE
range.size(c(14.3, 5.6, 22.1)) == 16.5	# TRUE
# yes! my function works.

# Question 44
diff.height <- range.size(AgeWeight$HEIGHT)
diff.weight <- range.size(AgeWeight$WEIGHT)
diff.bmi <- range.size(AgeWeight$BMI)
data.frame(diff.height, diff.weight, diff.bmi)

# Question 45
AgeWeightDifferences <- data.frame(diff.height, diff.weight, diff.bmi)	# store results
?write.csv
write.csv(AgeWeightDifferences, "age_weight_differences.csv")		# saves the data frame in a .csv file
