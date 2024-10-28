# Self guided tutorial 
# Week 6 Part 2

library(ggformula)


# Please use these two line to download and read into memory the file "kung_heights.csv", and to 
# create a dataframe of adult heights.

d <- read.csv(url("http://hadzasounds.com/shiny/data/kung_heights.csv"), stringsAsFactors=F)
da <- d[d$age>=18,]

# Exercise 6.8
#
# Next, I'd like you to practice filtering data, by creating two new data frames, one which holds only male
# adults heights, and one which holds female adult heights. 

# please complete these two code stubs:

male_data <- filter(da, male == 1)
  female_data <- filter(da, male == 0)
  
  # Exercise 6.9
  # Next, calculate the means for the men, and for the women.
  
  # please complete these two code stubs:
  
  male_mean <- mean(male_data$height)
  female_mean <- mean(female_data$height)
  
  # Exercise 6.10
  # Next, calculate the SEM for males and females
  
  
  
  
  # Exercise 6.11
  # Next, calculate the 95% confidence intervals for the male sample mean and the female sample mean
  
  


# Exercise 6.12
# Next, plot a histogram for males with its sample mean and 95% confidence interval displayed. 
# Please make sure that each histogram has 40 bins and an x-axis ranging from 130 to 180 (this
# is set by tacking on '+ xlim(130, 180)' to the very end of your code creating the histogram).


# Exercise 6.13
# Next, plot a histogram for males with its sample mean and 95% confidence interval displayed. 
# Please make sure that the histogram has 40 bins and an x-axis ranging from 130 to 180.



# Exercise 6.14
# Do the sample means or their confidence intervals overlap? 


# What does this tell you about male and female heights?

# Example 6.15
# I'd like to show you two useful ways to represent differences between sub-groups in the data.
# The first measure is the raw difference, in the standard units, in this case, cm.
# So here we calculate the difference between the male mean and the female mean:

m_f_difference <- male_mean - female_mean

# Example 6.16
# Another way to express differences between subgroups is in keeping the spirit of 'z-scores'. 
# Here, we standardize the difference in terms of units of standard deviation, rather than centimeters.
# The first thing we do is we calculate the standard deviation for the entire dataset, or the "pooled data"
# as it is often referred to. 

pooled_sd <- sd(da$height)

# Here is the difference between males and females, expressed in terms of standard deviation units. 
# This standardized measure of difference, or effect size (in the sense of 'the effect of being male') is known as "Cohen's D". 

cohens_d <- m_f_difference/pooled_sd

# what is the effect size, in terms of Cohen's D? Write you answer in the comments here:
# The effect size is: 

# Example 6.17
# Here is how you fit a linear model that is empty, i.e., has just the intercept, that is, the mean height across
# samples.

m1 <- lm(height~1, data=da)

# Example 6.18
# Here is how you view the estimate for the intercept / population mean. Look at the line that says (Intercept).
# Look at the value for "Estimate". You can also see here a value for "Std. Error". 

summary(m1)

# Exercise 6.19
# Is the value reported here for the mean the same as the value you reported in part 1 of this tutorial (the mean adult height?)
# Is the value reported for the Std. Error of the intercept the same as the value you calculated in part 1 of this tutorial?


# Example 6.20
# Here is how you fit a linear model with one predictor variable, in this case, sex.

m2 <- lm(height~male, data=da)

# Example 6.21 Please look at the summary for m2, and look at the values now appearing in the 
# column "Estimate" for the rows (Intercept) and male.

# Exercise 6.22:
# Q: How does the intercept value for m2 differ from that of m1?


# Exercise 6.23
# Q: How does the intercept value of m2 compare to that of female mean height, calculated already?


# Exercise 6.24
# How does the value for "male" compare to the value of the difference between male and female average heights?

