#use the library function to call these four pkgs
library(ggformula)
library(Lock5withR)
library(dplyr)
library(mosaic)

# Part I: Viewing data frames and data frame variables
# For this part, we will be looking at the data set ACS, which
# provides data on a sample of individuals in the American Community Survey

ACS
Lock5withR::ACS


# Notice that it is somewhat difficult to succinctly view the columns and
# rows when you call the entire data set, so it's useful to use the head()
# function to get a quick peek at the data

head(ACS)

#You can use $ to specify a particular column/variable of a data frame. The
#below code returns a vector with the items located in that column/variable

ACS$Race
head(ACS$Race)

#You can use [] to specify a particular item in a vector

ACS$Race[1]

#You can specify a sequence of items in a vector by using :
ACS$Race[1:4]

#You can create a vector of numbers corresponding to which items you want to
#specify from another vector by using c()
ACS$Race[c(1, 2, 4)]

# Part II: Variable types (quantitative vs. categorical, 
# numeric/character/boolean)

# Main variable types in R: numeric, character, boolean. There are 
# also integer, complex, and raw types, but don't worry about those 
# for now

a <- 5.1
print(class(a))
b <- "hello"
print(class(b))
c <- TRUE
print(class(c))

# The two ways variables are measured in R: quantitative and categorical
#quantitative - values represent quantity, cat=vars rep category
# Let's look at how the variables from ACS are measured using str()
str(ACS)

# R identifies the variable "Sex" as an integer, but we know that for this
#survey, it is a categorical rather than a quantitative variable. We want R
#to identify Sex as a category so we will use factor() to assign labels 
#to the levels (values) of Sex to denote the categories and look at the new structure
ACS$Sex
str(ACS$Sex)
ACS$Sex <- factor(ACS$Sex, levels = c(0, 1), labels = c("male", "female"))
str(ACS$Sex)

# Part III: Manipulating data frames (filtering data, recoding variables, 
# creating new variables, etc.)
# ACS$HoursWk has a lot of missing values (NAs) - we can use the filter()
# function to only look at the rows of ACS with HoursWk values that exist
filter(ACS, HoursWk != "NA")

#We can also create new variables of ACS, for example, a summary variable that 
#indicates via true or false whether or not a person/observation works long
#hours, defined as HoursWk > 40

ACS$WorksLongHours <- ACS$HoursWk > 40
ACS$WorksLongHours
#Don't have to use if()

# Part IV: Exploring variation in data using distributions (histograms, boxplots, etc.)

#We can explore distributions of quantitative variables using gf_histogram()
#which shows us on the y axis the number of observations that fit 
#each x value
#gf_dhistogram will show us on the y axis the proportion of all values 
#that fit each x value

gf_histogram(~ Age, data = ACS)
gf_dhistogram(~ Age, data = ACS)

#favstats() gives us the five-number summary of a quantitative variable
favstats(ACS$Age)

#gf_boxplot is another way to explore a distribution in terms of its IQR
gf_boxplot(Age ~ 1, data = ACS)

#For categorical variables, the best way to explore distributions is using
#the tally() function. If a categorical variable is recognized by R as a
#factor, the gf_histogram function will not work. 
tally(~ Race, data = ACS)
gf_histogram(~ Race, data = ACS)
#Note the error in the above function - why is this?


ACSsex <- ACS$Sex
ACS$Sex <- ACSsex
