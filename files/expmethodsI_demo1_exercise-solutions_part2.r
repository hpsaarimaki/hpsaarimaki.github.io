# Demo 1, Part II
# Preparing your data for statistical analyses
# Solutions to exercises
# Heini Saarimäki, 18.9.2014

# -----

# 1. Load in the data

# Navigate to the correct directory
getwd()				# check in which directory you are now
setwd("Z:/Desktop/Demo1")	# change the working directory to point to the folder where you want to save your work
dir()					# list the contents of your working directory

# Load the data into a data frame 'naming'
naming <- read.csv("http://bit.ly/RotCjb", header=TRUE, sep="\t")	# use ?read.csv to check why we added the argument header=TRUE 

### Question 1:

?read.csv
# "header" specifies whether the file contains the names of the variables as its first line
# "sep" specifies the field separator; here it's tab

### Question 2:
names(naming)			# get the column names
# column names: X, iq, hrs, word.type, ms, male

## Question 3:
head(naming)			# check the first 6 rows of the data
dim(naming)				# check the dimensions of the data
# the data consists of 2000 observations (rows) and 6 variables (columns)
# variable 'iq' might have something to do with IQ...
# variable 'hrs' might be related to hours...
# variable 'word.type' is coded as a character...
# variable 'ms' might have something to do with time in milliseconds...
# variable 'male' seems to take in only 0's and 1's and might be related to sex...

# ---

# 2. Factors

## Question 4:
summary(naming)			# displays all the different variables
summary(naming$hrs)		# use these to display the summary of just one variable at the time
summary(naming$word.type)
# The two summaries are different, because the two variables are of different data types:
# variable 'hrs' is a continuous, numeric variable
# variable 'word.type' is a factor
# Remember that when we read in the data, R automatically "guessed" the data type of each variable:
# in the case of numbers (e.g., hrs) it interpreted the data as being numeric
# in the case of characters (e.g., word.type) it interpreted the data as being factor
# Check the demo slides if this is unclear!

## Question 5
class(naming$hrs)			# class function returns the data type: numeric in case of 'hrs'...
class(naming$word.type)		# ... and factor in case of 'word.type'
# So as stated already in Question 3, 'hrs' is numeric and 'word.type' is factor

## Question 6
summary(naming)			# hmm... variable 'male' looks suspicious
head(naming)			# check the coding again
# It seems that the variable 'male' represents categories (male and female coded as 0's and 1's)
# The summary shows mean for 'male', but this is not possible for a categorical variable
# This might cause difficulties in future analyses

class(naming$male)		# checking the data type of variable 'male'
# The data type of variable 'male' is numeric (integer here), this explains the summary results
# Remember that when we read in the data, R automatically guessed the data type of each variable:
# in the case of numbers it interpreted the data as being numeric
# But in the case of 'male' this is a wrong guess - the variable is a factor
# You have to explicitly tell R that 'male' is a factor

## Question 7
naming$male <- factor(naming$male)  # change 'male' to a factor
summary(naming$male)			# check the summary now
class(naming$male)			# and the data type
# summary and class confirm that we have now successfully changed 'male' to a factor

## Question 8
# Let's add textual labels to the factor. 
# I will use 'female' and 'male', but it could be anything you prefer (e.g., 'f', 'm')
naming$sex[which(naming$male==1)] <- 'male'
naming$sex[which(naming$male==0)] <- 'female'
naming$sex <- factor(naming$sex)
# We add a column 'sex' and immediately assign to it 'male' for rows where male==1...
# ... and 'female' if for rows where male==0
# Finally, we changes the variable into a factor.
head(naming)				# check how it went...
# We now have a new column where sex is coded with textual labels. 
# Notice also this:
summary(naming$sex)
head(naming$sex)
# Taking the values of the variable 'sex' with, e.g., head displays also the levels of the factor.
# Same happens for the other factor, 'word.type':
head(naming$word.type)
# Finally, as variables 'male' and 'sex' now contain the exactly same information, we remove the old 'male':
naming$male <- NULL
summary(naming)

# Our variables - at least data types - now seem to be ready for analyses!
# At this point, we can make our life easier by using attach:
attach(naming)
?attach			# check what the function does
# I will leave it to you to explore what attach does, we will use it a lot in future.

# ---

# 3. Missing values

## Question 9
summary(naming)			# here we go again with the useful summary function...
# Now take a closer look to the values.
# Do the min and max values make sense?
# Everything else seems ok, but it seems that there are some odd values in IQ...
# If you think of how IQ works, it can never* be over 200.
# But we do have some values higher than that in IQ! Let's take a closer look:
max(naming$iq) 			# displays the minimum value of 'iq'
boxplot(naming$iq)		# we can plot 'iq' and see there are some cases with value 999
					# more on plotting next week!
# The coding of 'iq' as 999 is either a typo or a funny coding for a missing value.
# We want to code these explicitly as missing values!
# R has a way to code a missing value, it uses NA.

## Question 10
which(naming$iq==999)		# locate the 999 values of 'iq'
# Notice the use of a conditional operator (==) here!
# The observations number 33, 278, 1033, 1278 have value 999.
# Code these observations as missing values (NAs):
naming$iq[which(naming$iq==999)] <- NA
# Check what happened to the observations:
naming[c(33, 278, 1033, 278),]
# The 'iq' for these observations is no coded as NA, other variables remain the same.
# Check what happened to summary of 'iq':
summary(naming)
# Summary now displays the number of NAs per each variable.

## Question 11
# Normally you know your data so you know this, but sometimes you don't have a complete record of how the data was collected.
nrow(naming)			# check how many observations in the data
summary(naming)			# good to check this again...
# This give 2000 observations.
# BUT notice that variable 'word.type' has two experimental conditions.
# Maybe all participants went through both conditions (exception and regular)?
# This has to be checked. Let's compare the first 10 and "second" 10 observations:
naming[1:10,]
naming[1001:1010,]
# Importantly, note that the background variables 'iq', 'hrs', and 'sex' are the same..
# ... it's quite unlikely that this is a coincidence. 
# Indeed, the first 1000 rows (1:1000) and second 1000 rows (1001:2000) represent the same participants.
# The only changes are the condition (word.type) and value related to this condition (ms).
# This way, we notice that there are only 1000 participants.
# Of these, 2 had missing values (remember that the missing values were also coded twice!).
# You can also check the missing values by using is.na function:
naming[is.na(naming$iq),]
# So our data includes a complete record of 998 participants.

# ---

# 4. Sampling

# For practicing purposes, we will test how to take a random subsample of our data.
# IMPORTANT! Remember this 'sampling' is a different concept of what was explained in Lauri's lecture.
# Usually, you do plan your sampling carefully BEFORE collecting your data and collect data just from your sample.
# This is done partly because data collection is expensive.

# In R, sampling simply refers to taking a subset (part) of your data.
# Sometimes you work with large datasets (e.g., free data from internet) and want to take a smaller chunk of it.
# This is what we will practice next.

## Question 12
naming$id <- factor(rep(1:1000, times=2)) # add factor 'id' with participant numbers
head(naming)					# check results
naming$id						# check even further...
# We created the subject id's based on the observation that each of the 1000 participants is included twice.

## Question 13
?sample						# check how this function works
my.sample <- sample(1000,100)			# take the sample
my.sample						
my.sample <- sort(my.sample)			# sort it in ascending order (see ?sort)
my.sample

## Question 14
# Let's try it this way first:
naming_sample <- naming[my.sample,]		# here we just take the rows according to our sample
# Ok, it worked, but is it correct?
summary(naming_sample)
dim(naming_sample)
# Oopsie! This would sometimes work, but in this case our dataset had the same participants twice.
# So including the sample of rows just included the sample from first 1000 observations...
# ... so it actually just included the regular 'word.type' conditions (see summary).
# And we see that number of rows in the sample is 100, but it should actually be 200 (twice for each participant).
# So this is not the way to do it!
# A better solution (one of them):
naming_sample <- naming[c(my.sample, (my.sample+1000)),] 
summary(naming_sample)
dim(naming_sample)
# An even better solution ("a more R way") uses %in% operator. Check it out: ?%in%
naming_sample <- naming[naming$id %in% my.sample,]
summary(naming_sample)
dim(naming_sample)

# -----

# Please send your questions or comments to heini.saarimaki@aalto.fi!

# -----
