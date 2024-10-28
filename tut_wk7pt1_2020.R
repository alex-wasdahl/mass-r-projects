# Tutorial 
# Week 7 Part 1
# Linear Regression with Binary Predictor Variable (Height and Sex)
# Measures of effect size (raw difference, Cohen's D)
# 
# Tutorial Setup
library(ggformula)

# Please use these two line to download and read into memory the file "kung_heights.csv", and to 
# create a dataframe of adult heights.

d <- read.csv(url("http://hadzasounds.com/shiny/data/kung_heights.csv"), stringsAsFactors=F)
da <- d[d$age>=18,]

# It is always good to peek at your data, so run this line to see the columns and some of the rows
head(da)

# Please note, this tutorial builds upon Tutorial 6 Part 1, in which you calculated the
# mean !Kung adult height, and its standard error.
# Please open that tutorial code file first, and port some of its results here. 

# Write the mean adult height here (from exercise 6.2):
mean_adult_height <- mean(da$height)
  
  # And write the standard error of the mean adult height here (from exercise 6.5):
  adult_height_sem <- sd(da$height)/sqrt(nrow(da))
  
  
  
  # Exercise 7.1
  #
  # Here, I'd like you to practice filtering data, by creating two new data frames, one which holds only male
  # adults heights, and one which holds female adult heights. 
  
  # please complete these two code stubs:
  
  male_data <- 
  female_data <- 
  
  # Exercise 7.2
  # Next, calculate the mean height for the men, and the mean for the women
  
  # please complete these two code stubs:
  
  male_mean <-
  female_mean <-
  
  # Exercise 7.3
  # Next, calculate the SEM for males and females. 
  # Use the same formula that you used in Tutorial 6.1 and which should be shown on a lecture slide.
  # save your answers in variables called male_mean_sem and female_mean_sem
  
  
  
  # Exercise 7.4
  # Next, calculate the 95% confidence intervals for the male sample mean and the female sample mean
  
# What is the 95% confidence interval for male mean height? Write your answer in comments here.
#
#

# What is the 95% confidence interval for female mean height? Write your answer in comments here.
#
#


# Exercise 7.5
# Next, plot a histogram for male adult heights with overlayed vertical lines
# displaying the sample mean and 95% confidence interval displayed. 

# Exercise 7.6
# Next, plot a similar histogram for females with its sample mean and 95% confidence interval displayed. 



# Exercise 7.7
# Do the 95% confidence intervals of the male and female sample means overlap? 
# What does this tell you about male and female heights?
# Answer in comments below.


# 
#
#

# Exercise 7.8
# I'd like to show you two useful ways to represent differences between groups or sub-groups in data.
# The first measure is the raw difference, in the standard units, in this case, cm.
# So next please complete this code stub to calculate the difference between the male adult mean 
# and the female adult mean:

m_f_difference <- 
  
  # Example 7.9
  # Another way to express differences between subgroups is in keeping the spirit of 'z-scores'. 
  # Here, we *standardize* the difference and use units of standard deviation, rather than centimeters.
  # The first thing we need to do is calculate the standard deviation for the entire dataset, or the "pooled data"
  # as it is often called:
  
  pooled_sd <- sd(da$height)

# Exercise 7.10
# Here is the difference between males and females, expressed in terms of standard deviation units. 
# This standardized measure of difference, or effect size (in the sense of 'the effect of being male upon height') 
# is known as "Cohen's D". 

cohens_d <- m_f_difference/pooled_sd

# what is the effect size, in terms of Cohen's D? Write you answer in the comments here:
# The effect size is: 

#
#
#

# Exercise 7.11
# This is how you fit a linear regression model that is "empty", or more accurately, 
# just has a term for the mean height (i.e. the intercept)

m1 <- lm(height~1, data=da)

# Example 7.12
# Here is how you view the estimate for the intercept / population mean. Look at the line that says (Intercept).
# Look at the value for "Estimate". You can also see here a value for "Std. Error". 

summary(m1)

# Exercise 7.13
# Look at the value reported for the intercept, and the SEM.
# How does the intercept value reported by lm compare to the mean value you calculated for 'mean_adult_height'?

# How does the intercept's Std. Error reported by lm compare to the SEM value you calculated for 'adult_height_sem'?



# Example 7.14
# Here is how you fit a linear model with one binary predictor variable, in this case, sex.

m2 <- lm(height~male, data=da)

# Example 7.15 Please look at the summary for m2, and look at the values now appearing in the 
# column "Estimate" for the rows (Intercept) and male.

# Exercise 7.16:
# Q: How does the intercept value for m2 differ from that of m1? Write you answer below.
# A: 

# Exercise 7.17:
# Q: How does the intercept value of m2 compare to that of female mean height ('female_mean'), calculated already?
# A:

# Exercise 7.18
# Q: How does the value for "male" compare to the value of the difference between male and female average heights ('m_f_difference')?
# A: 

