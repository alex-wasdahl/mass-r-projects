# Tutorial 
# Week 7 Part 2

# Linear Regression with Continuous Predictor Variable
# Infant Mortality Rate and Total Fertility Rate

# Read in World Bank Data for 
# IMR and TFR,
# and plot the data

d <- read.csv(url("http://hadzasounds.com/shiny/data/tfr_imr_2018.csv"), stringsAsFactors=F)
plot(d$IMR_2018, d$TFR_2018, xlab="IMR", ylab="TFR", main="2018 Infant Mortality and Fertility Rates", xlim=c(0,90), ylim=c(0,7))
text(x=d$IMR_2018, y=d$TFR_2018, d$Country.Name, pos = 4, cex=0.75)
text(x=60, y=0, "World Bank DataBank", cex=0.75, col="blue")

# Exercise 7.19 
# Use Equation 3 shown on the lecture slide 
# to calculate the 
# slope of the regression line, b_1.
# decompose it into (A-B) / (C-D)

# here is part A
n_cases <- nrow(d)
x_times_y <- d$IMR_2018*d$TFR_2018 
sum_of_x_times_y <- sum(x_times_y) 
part_a <- n_cases*sum_of_x_times_y

# work here on part B

# work here on part C

# work here on part D

# now solve for  b_1
beta_1 <- (part_a-part_b)/(part_c-part_d)

# Tutorial Exercise 7.20

# Having calculated the slope of the regression line, you should now be able to calculate the 
# intercept of the line, b_0, using Equation 2 shown on the lecture slide.
# complete the code stub below:

beta_0 <-
  
  # Exercise 7.21
  
  # You have now calculated the linear model for the best-fit relationship between 
  # IMR and TFR, in these data. 
  # Using the slope and intercept terms, you should now be able to 
  # predict a TFR value for each case of your data, based on the best fit regression line. 
  # Forget for a moment that you have actually measured TFR for each country! 
  # Instead, just focus
  # on *generating model-based predictions for TFR*
  # Please complete the code stub below, 
# Equation 1 shown on the lecture slide to generate predicted TFR for two countries:
# Country A has a IMR of zero.
# Country B has an IMR of 80. 

IMR_values <- c(0,80)

# complete this code stub, using equation 1 on the lecture slide.
predicted_TFR <-
  
  # part 2
  
  predicted_TFR <- beta_0 + beta_1*IMR_values

# part 3-4

plot(d$IMR_2018, d$TFR_2018, xlab="IMR", ylab="TFR", main="2018 IMR and TFR", xlim=c(0,100), ylim=c(0,6.5))
text(x=d$IMR_2018, y=d$TFR_2018, d$Country.Name, pos = 4, cex=0.75)
text(x=80, y=0, "World Bank DataBank", cex=0.75, col="blue")
lines(x=IMR_values, y=predicted_TFR, lty=2, col="red")