#SOCSC402 Lab 11/8/21
#TA: Alex Wasdahl

#Load the following packages
library(ggformula)
library(Lock5withR)
library(dplyr)
library(mosaic)
library(supernova)

#Quiz 3 Debrief

#Q1
#Yi is the outcome or depedent variable for observation i
#B0 is the population parameter that represents the true mean
#ei is the residual or error (actual minus predicted) for each observation i
#See section 7.1 in book

#Q2
#The standard error of the mean (S.E.M) is the standard deviation of the estimate for the population mean.
#See Lecture 6 slide 22 or Lecture 5 slide 29

#Q3
the_url <- "https://drive.google.com/uc?export=download&id=1ZC8PSy-cyLWyo7fOt8sjiUiPPHGIqDZc"
d <- read.csv(url(the_url), stringsAsFactors=F)
#Filtering data: see lecture 6 slide 5 and/or week 6 tutorial and/or 10/25 Lab
da <- d[d$age>=18,]
na <- nrow(da)
da_weights <- da$weight
#SEM: see Lecture 6 slide 22 or Lecture 5 slide 29
mean_da_weights <- mean(da_weights)
sd_da_weights <- sd(da$weight)
sqrt_na <- sqrt(na)
semQ3 <- sd_da_weights/sqrt_na
mean_da_weights
semQ3

#Q4
#a) Empty Model: Section 5.3, previous labs/lectures
WeightEmpty <- lm(weight ~ NULL, data = d)
WeightEmpty
#35.61
#b) SEM: see Lecture 6 slide 22 or Lecture 5 slide 29
mean_d_weights <- 35.61
sd_d_weights <-sd(d$weight)
n <- nrow(d)
sqrt_n <- sqrt(n)
semQ4 <- sd_d_weights/sqrt_n
mean_d_weights
semQ4
#c) Confidence Intervals: Lecture 6 slides 24-25 and/or Week 6 Tutorial
minweightCI <- mean_d_weights - 1.96*semQ4
maxweightCI <- mean_d_weights + 1.96*semQ4
minweightCI
maxweightCI
#(34.37308, 36.84692)
#d) Sum of Squares: Section 6.0, previous labs/lectures
anova(WeightEmpty)
#117643

#---------

#Linear Models: Categorical and Quantitative Explanatory Variables
#Previous lab looked at linear models with categorical expl. variables
#Baseball example: using wins as the outcome variable and home run categories as explanatory
BaseballHits$PowerTeam <- BaseballHits$HomeRuns > median(BaseballHits$HomeRuns)
WinsPowerModel <- lm(Wins ~ PowerTeam, data = BaseballHits)
Power3Groups <- ntile(BaseballHits$HomeRuns, 3)
BaseballHits$PowerTeam3Groups <- factor(Power3Groups, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
WinsPower3GroupsModel <- lm(Wins ~ PowerTeam3Groups, data = BaseballHits)
supernova(WinsPowerModel)
supernova(WinsPower3GroupsModel)
#Adding more groups explains away more error, but at the cost of "spending" degrees of freedom
#Two-group model: 𝑌𝑖=𝑏0+𝑏1𝑋𝑖+𝑒𝑖
#DATA = MODEL + ERROR: In the group models w/ CEVs, MODEL was represented by group means
#Now for linear models with quantitative EVs, MODEL will be represented by the regression line
#Regression model: 𝑌𝑖=𝑏0+𝑏1𝑋𝑖+𝑒𝑖
#y = mx+b
#Xi is now a continuous rather than a binary "on-off switch"
#In the group model, b1 is the increment between group means, added only when 𝑋𝑖 is equal to 1.
#In the regression model, b1 is the slope of the regression model
#b1 in the regression model is the increment to predicted Y for each one-unit increment in X
#b0 in the group model is the mean of the base group (e.g. PowerTeamFALSE)
#b0 in the regression model is the y-intercept
#In both cases, b0 represents the value of Y when X = 0
WinsHomersModel <- lm(Wins ~ HomeRuns, data = BaseballHits)
WinsHomersModel
# Yi = b0 + b1Xi + ei
# Yi = 58.9 + 0.14Xi + ei
# Yi represents wins (observed value of outcome variable for obs i), 
# Xi represents homers, b0 is predicted wins w/ 0 homers, 
# b1 is wins to add for each homer 
# ei is residual (difference between the prediction based on the model, 
# and the actual value)
gf_point(Wins ~ HomeRuns, data = BaseballHits) %>%
  gf_lm(color = "magenta")
# Function to predict future scores: ^Yi = 58.9 + .14Xi
BaseballHits$PredictedWins = predict(WinsHomersModel)
head(BaseballHits)
#Scatterplot of predicted wins by home runs
gf_point(PredictedWins ~ HomeRuns, data = BaseballHits)
#Residual = diff b obs and exp wins
BaseballHits$Residuals <- resid(WinsHomersModel)
head(BaseballHits)
gf_histogram(~ Residuals, data = BaseballHits, bins = 5)
# the sum of the residuals from each score to the regression line add up to 0, by definition
EmptyWinsModel <- lm(Wins ~ NULL, data = BaseballHits)
supernova(EmptyWinsModel)
supernova(WinsPowerModel) #2 group model
supernova(WinsPower3GroupsModel) #3 group model
supernova(WinsHomersModel) #regression model
