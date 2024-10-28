#Functions in R take in values or "arguments" passed in by the user and return a particular output
sum(5, 10, 15)

#You can store the results of functions into variables in R
sumresult <- sum(5, 10, 15)
sumresult

#the print function can be used to print character strings which are encased in quotation marks
print("hello")
welcomemessage <- "hello"
print(welcomemessage)

a1 <- print("hello")
a1

#Vectors in R are the simplest form of data structure that holds elements of primitive data types
# The c() function combines its arguments to form a vector
my.vector <- c(1,2,3,4,5)

#Multiplying a vector by a number will multiply each of its elements by that number
my.vector * 100

# Data types: 
# Numbers
mynumber <- 12
# Text (characters)
mytext <- "abc"
# Booleans (True, False)
mybool <- TRUE
mybool2 <- F

#use the install.packages() and library() functions to load the Lock5withR package
install.packages("Lock5withR")
library(Lock5withR)

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

#If you know you will be making changes to columns/variables in a data set, and do not want
#to risk making a mistake and having to remove and reinstall packages, you can store a
#data frame column in an independently defined variable
ACSincome <- ACS$Income
#Now if you make a mistake, you can just redefine the ACS$Income column itself and
# store the ACSincome variable into it 
ACS$Income <- ACSincome