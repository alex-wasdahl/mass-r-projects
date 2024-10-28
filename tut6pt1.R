# Self guided tutorial 
# Week 6 Part 1

library(ggformula)

# Let's practice calculating the standard error of the mean, using as our data source
# the heights of !Kung adults, measured by Nancy Howell in the 1960's and 1970's. 
# Please use this line to download and read into memory the file "kung_heights.csv" stored on my server. 

d <- read.csv(url("http://hadzasounds.com/shiny/data/kung_heights.csv"), stringsAsFactors=F)

# Next, take a peak  at the data using 'head':
head(d)
# You'll note that in this data coding scheme, male = 0 means a female, and male = 1 is male.
# the heights are in cm, and the weight in kg.

# Let's now calculate the mean height of adult !kung adults of either gender.

# Exercise 6.1:
# Please filter the data so that you only have adult heights (those aged 18 and above)
# Save the filtered data in a new dataframe called 'da'

# Complete the code stub below

da <- filter(d, age > 18)
  
  # Exercise 6.2:
  # I now assume that you have a dataframe 'da' that contains only adult heights.
  # please calculate the mean adult height. Save this value in a variable called 'mean_adult_height'
  
  # Complete the code stub below
  
mean_adult_height <- mean(da$height)
  
  
  # Exercise 6.3:
  # Please plot the height data using a gf_histogram with *40 data bins.*
  
  
  
  # Exercise 6.4: 
  # Do the data look normally distributed or not? Why or why not?
  # Write down your answer in comments here:
  
#
#

# Example 6.5:
# Please calculate the standard error of the mean of adult height, following the 
# Formula SEM = sample standard deviation / square root of sample size. Save the
# standard error of the mean in a variable called 'sem'. 

  sem <- sd(da$height)/sqrt(nrow(da))

# Example 6.6
# Please recall that the SEM represents the standard deviation of the estimate of the population mean.
# With that in mind, we can apply the empirical rule to create confidence intervals on our
# estimate of the population mean adult height (95% CI for 'mean_adult_height').

# According to the *empirical rule*, we can be 95% confident that the population mean height will be located
# within 3 standard deviations of the mean (+/-).

# Please calculate the minimum height value and the maximum height value that 
# represent our 95% confidence interval for mean adult height. 

# Complete the code stubs below

min_height_95_ci <- mean_adult_height - 3*sem
  max_height_95_ci <- mean_adult_height + 3*sem
  
  
  # Exercise 6.7 
  # Now for some graphics / plotting practice.
  # It is possible to add lines and text as annotation to r plots, and this can often be helpful for labeling
  # cases, or important thresholds, or adding Simpson's characters to a plot (maybe for another lecture). 
  # In this example, I'd like to practice adding vertical lines to the gf_histogram that represents the
  # estimate of the population mean, and its 95% confidence interval.
  gf_histogram(~height,data=da,bins=40,xlim=c(130,180))%>%
    gf_vline(xintercept = ~mean_adult_height,color="blue")%>%
    gf_vline(xintercept = ~min_height_95_ci,color="magenta")%>%
    gf_vline(xintercept = ~max_height_95_ci,color="magenta")
  # To add a vertical line, you use the function 'gf_vline'. 
  # To get examples of how this is used, type ?gf_vline into the console (the bottom of that help page 
# has examples of gf_vline being used). 

# So now please plot a gf_histogram that includes the distribution of the heights (as you already did above)
# but now also includes a blue vertical line for the estimate of the population mean,
# and two red lines for the 95% confidence intervals of that estimate. 
