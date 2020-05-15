# Experimental and Statistical Methods in Biological Sciences I
# Demo 1: Basics of using the R software
# Heini Saarimäki, 18.9.2014

# -----

# 1. Getting started

# ---

# 1.1 Command line syntax

# We are now in R editor.
# Write/copy commands to R console and run the commands by pressing ENTER.
# Or by pressing CTRL+r in the row you want to run.

# Mark a comment with #.
# In R console, the > is the prompt. 
# The [1] is a counter (not part of the data).

3*(2+2)	# A simple example.

# --

# 1.2 Variables and assignments

# Start with a simple example:

x <- 3

# Here, we *assign* a *value* 3 to *variable* x.
# Variables are names that refer to temporarily stored values. 
# Variable names can contain letters, numbers, '_' and '.', but cannot begin with a number. 
# Variable names are also case-sensitive.
# Assignment operators are <- and =.

# More examples:
x <- 3	# x becomes 3, i.e. assign value 3 to x
x		# prints its own current value
x = x+1	# x becomes x+1, its previous value is lost
y = x+1	# y becomes x+1, x is unchanged

# --

# 1.3 Functions, arguments, and returned values

# Data structures store your data.
# Functions process it.

# Think of cuntions as operators: like + and - but more specialized.
# Basic idea: return value = function(argument)
# A function is written once but may be called many times, each time with different arguments.
# A function returns a value, the result of the operation.

# You call a function by typing its name followed by brackets containing the arguments.
# You assign the returned value to a variable for further processing:

x <- 9		# prepare function input
y <- sqrt(x)	# call function sqrt() with argument x, returned value is assigned to y

# -

# 1.3.1 Finding functions in R

# Search for function names:

help.search("read")	# search all
apropos("^read")		# names starting with "read"
apropos(\\.test$)		# names ending with ".test"

# Function help:

help.start()		# manuals and reference
help(t.test)		# help page for function "t.test"
?t.test			# ...same as above

# -

# 1.3.2 Packages

# A package is a collection of previously programmed functions.
# Using packages is essential in effective use of R.

# You need to first install a package to the base version of R.
# And then to load the function for your current R session.

# Useful package-related commands:

sessionInfo()			# packages loaded into memory
library()				# list packages installed on your computer
install.packages("psych")	# download and install package "psych"
library("psych")			# load package named "psych"
library(help="psych")		# list the functions in "psych"
update.packages()			# updates all your installed packages

# -

# 1.3.3 Basic built-in functions

getwd()					# get working directory
setwd("C:/Desktop/Stats/Demo1")	# set path to working directory
dir()						# list files in working directory
ls()						# list objects in workspace
rm(list=ls())				# remove all objects in workspace (clear)
graphics.off()				# close all figures

# -----

# 2. Data types

# Three main data types in R:

# --

# 2.1 Numeric: 

111

# --

# 2.2 Character: 

"apple"

# --

# 2.3 Logical

3 > 2				# is 3 greater than 2?
"apple" > "orange"	# compared alphabetically
"Apple" > "apple"		# case-sensitive

# Conditional operators:
x == y			# x equal to y?
x != y			# x not equal to y?
x < y				# x less than y?
x =< y			# x less than or equal to y?
x > y				# x greater than y?
x >= y			# x greater than or equal to y?

# -----

# 3. Data structures

# --

# 3.1 Vector

# Vector is an ordered row or column of cells.
# Each cell contains a single value.
# Cell values must all be of the same data type.

# Functions that make vectors:

c(1,3,4.5)				# combine vectors
c("apple", "apple", "orange")	

1:5					# sequence of numbers (integer steps) with :
5.5 : -5.5

seq(0, 2*pi, by=0.1)		# sequence of numbers (fractional steps)
seq(0, 2*pi, length(64))

rep(1:3, times=3)			# repeat a vector
rep(1:3, times=c(3,5,2))
rep(1:3, each=3)

# --

# 3.2 Arrays and Matrices

# While vector are one-dimensional, arrays are a multidimensional extension of vectors.
# Most commonly used array in R is the matrix.
# Matrix (in R) is a 2-dimensional layout of cells.
# Cell values must all be of the same data type.

# Functions that make matrices:

x = 1:12						# set up the example
	
matrix(x, nrow=3, ncol=4)			# converts a vector to a matrix, by columns dy default
matrix(x, nrow=3, ncol=4, byrow=TRUE)	# this does it by rows
		
cbind(x,x)						# bind vector into a matrix by columns
rbind(x,x)						# ...or by rows

# --

# 3.3 Data frames

# We can stores our empirical data in a matrix, with rows representing observations and columns representing variables.
# A data frame is like matrix...
# ... but it is better: easy access to elements
# ... and more flexible: variables/columns can be of different data types.
# In R, data frames are the preferred method for working with observations and variables -type datasets.

# Functions tha create data frames:

x = 1:12						# set up the example
y = 13:24	

data.frame(x,y)

read.csv("ageweight.csv")			# read .csv data into a data frame
read.csv("http://bit.ly/RNEFpX")		# download online data
ageweight <- read.csv("http://bit.ly/RNEFpX")	# save the data for the session
write.table(ageweight, "ageweight.txt")	# export data
read.table("ageweight.txt")			# read text data into a data frame

save(ageweight, "ageweight")			# save R data objects
load("ageweight")					# load R data objects

# --

# 3.4 Lists

# List is a collection of objects.
# The components of a list can be different types of objects and can be of different lengths.
# Lists are used to pass structured arguments to functions...
# ... and to return multi-valued objects from functions.

mylist = list(fruit=c("apple", "orange"), price=c(1.2, 1.4, 0.8, 1.0))

# You will see a lot of lists when using more complicated functions.

# --

# 3.5 Basic diagnostic functions

class(x)		# gives the object class of x
str(x)		# display structure of object x
dim(x)		# retrieve dimension (rows, columns) of x
nrow(x)		# number of rows present in x
ncol(x)		# number of columns present in x
names(x)		# get/set column names of x
head(x)		# returns the first part of x (default 6 first)
tail(x)		# returns the last part of x

# -----

# 4. Subcripting

# For objects that contain more than one element, use subscripting to access the elements.
# Fast and efficient!
# Types of subscripts in R: numeric, character, logical

# --

# 4.1 Numeric subscripting

# Cells in the vector are ordered.
# The first element is addressed by subcript 1.
# Use square brackets: 
x[i]
# Here, x is the vector we want to subscript.
# And subscript i can be a single number or a vector of subscripts.
# Cells are returned in the order given in the subscript.

x = 1:12		# create a sequence of numbres from 1 to 12
x[1]			# first cell (x[0] is undefined)
x[length(x)]	# last cell
x[1:6]		# first 6 cells
x[6:1]		# first 6 cells (reversed order)
x[c(3,5,1)]		# cells 3, 5, 1 in that order

# Matrix and data frame cells have pairs of subcripts:
x[i,j]		
# Here, i is the row subscript and j is the column subscript.
# A rule of thumb: Rows first, then columns.

# Examples:

x = 1:12			# set up the example
y = 2*x	
z = 3*x
df <- data.frame(x,y,z)

df[2:3,]			# an empty index is shorthand for a complete index
df[,2:3]
df[2:3]			# a single subscript returns data frame columns by number
df[[2]]			# double-brackets return single columns in their own data type
df[-(2:3),]			# negative subscripts return all cells NOT subscript (remove!)

# Subscript vectors work on the left-hand side of assignments:
df[2:3,2] = -9		# set cells to -9
df

# --

# 4.2 Character subscripts

# Columns of a data frame, and components of a list, can be addressed by name.
# Use $ sign.
# Read in example data:
data <- read.csv("http://bit.ly/RNEFpX", header=T, sep=",")
	
names(data)		# get the column names
data$AGE		# get the column named AGE

# Use character vector to address columns by name:
data[,c("AGE","WEIGHT")]	# same as data[,2:3]

# --

# 4.3 Conditional subscripting

# Get or set a subset of data based on a condition.
# Logical index vector specify a pattern of cells to be addressed:
# Cells corresponding to cells of the logical vector that are TRUE.

data$SEX == 1		# logical vector (conditional)
data[data$SEX==1,]	# get rows where sex == 1
data[data$SEX==1,"AGE"]	# get AGE where sex == 1

# Logical vectors may be combined using operators & (AND) and | (OR) to form composite conditions:
data[data$SEX==1 & data$AGE>50,]	# get rows where sex == 1 AND age > 50

# -----

# 5. Operations on your data

# --

# 5.1 Vector arithmetic

# The arithmetic operators are: + - * / ^ %% 
# Unary operators (-x and x^2) apply element-wise.
# Binary operators apply pair-wise.
# (If the vectors are different lenghts the shorter is "recycled" to match the length of the longer).
# Scalar values are single-element vectors, recycled as necessary.

x <- c(0,2,4,6,8,10)
y <- c(1,3,5,7,9,11)
-y
y^2
y+2
y^2

# --

# 5.2 Vectorized functions

# Arithmetic functions take vector arguments and return vector values.
# The arithmetic operation is applied element-wise to the vector argument.

# Example functions:
a <- -5.6
round(a)		# round to given number of decimal places
ceiling(a)		# round values up
floor(a)		# round values down
abs(a)		# absolute value
sqrt(abs(a))	# square root
exp(a)		# exponential
log(abs(a))		# log to base e, see also log2(), log10()
sin(a) 		# trigonometric functions also: cos(), tan()
atan(abs(a))	# inverse trigonometric functions also: asin(), acos()
x
scale(x)		# scaling and centering: here, standardize as z-scores

# --

# 5.3 Descriptive functions

# Descriptive functions return a single-valued summary of a vector.

x <- seq(0,1,length=10)
length(x)		# number of elements in a vector
sum(x)		# sum of the values in a vector
min(x)		# minimum, see also max() and range()
mean(x)		# mean of the values in a vector
median(x)		# median of the values in a vector
sd(x)			# standard deviation, see also var()
cor(x)		# Pearson correlation, see also cov()

# Descriptive functions cov() and cor() return either a single value or a matrix...
# If the arguments are two vectors, a single value is returned.
# If the argument is a matrix, a matrix is returned.
# The i,j'th element of the matrix is the covariance/correlation between the i'th and j'th column vectors.

x <- rnorm(100)	# check out ?rnorm
y <- rnorm(100)	
cov(x,y)		# scalar covariance
cor(x,y)		# pearson correlation coefficient
cov(cbind(x,y))	# covariance matrix
cor(cbind(x,y))	# correlation matrix





