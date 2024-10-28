#load the following packages
library(ggformula)
library(Lock5withR)
library(dplyr)
library(mosaic)

#Where we left off from last time: If a categorical variable is recognized by R as a
#factor, the gf_histogram function will not work. 
tally(~ Race, data = ACS)
gf_histogram(~ Race, data = ACS)
str(ACS$Race)

#But if you want to display counts of categorical variables in such a manner, you can always use a bar graph
gf_bar(~ Race, data = ACS)

#Categorical explanatory, quantitative outcome --> faceted histogram, scatterplot (gf_point), jitter plot, boxplot
gf_histogram(~ HoursWk, data = ACS) %>%
  gf_facet_grid(~ Sex)
gf_point(HoursWk ~ Sex, data = ACS)
gf_jitter(HoursWk ~ Sex, data = ACS)
gf_boxplot(HoursWk ~ Sex, data = ACS)

#Categorical explanatory, categorical outcome --> frequency table, faceted bar graph
tally(Married ~ Race, data = ACS)
gf_bar(~ Married, data = ACS) %>%
  gf_facet_grid(~ Race)

#Quantitative explanatory, quantitative outcome --> scatter plot, jitter plot
gf_point(HoursWk ~ Age, data = ACS)
gf_jitter(HoursWk ~ Age, data = ACS)

#Quantitative explanatory, categorical outcome --> (not there yet!)

#Exploring variation --> explaining variation
#Why do we want to model variation? We model variation in order to 
#1. explain it. Creating statistical models to explain outcome variability and thus understand the DGP
#2. predict future outcomes/samples
#3. guide changes to improve outcomes we are studying

#Today we'll be specifically looking at modeling a single outcome variable (empty model)

#General foundation: DATA = MODEL + ERROR

#We'll start here by looking at the BaseballHits data frame from the Lock5withR package
head(BaseballHits)

#This data frame contains stats about all 30 MLB teams from the 2019 season
#We'll look at team wins as an outcome, and see how well we can "explain" variation in wins 
select(BaseballHits, Team, Wins)

#First, we'll create an empty model based on our outcome variable Wins 
EmptyWinsModel <- lm(Wins ~ NULL, data = BaseballHits)
EmptyWinsModel

# The value indicated by (Intercept) is a sample statistic, which is a parameter estimate
# It is based on our sample mean and is designed to predict the population mean
mean(BaseballHits$Wins)

#The quality of a model is based on how much error around the model is explained away
#The way to quantify the total error around the model is the Sum of Squared Deviations (SSD)
# Crudely copy/pasted equation: SSD = ∑(𝑌𝑖−𝑌¯)^2

# Each Yi is represented by each value in BaseballHits$Wins
BaseballHits$Wins

# Each Y¯ is represented by each predicted value of the empty model, which is the mean for each
# We can use the predict() function to generate these predicted values
predict(EmptyWinsModel)
#In this empty model, we can just repeat the number 81 30 times
rep(mean(BaseballHits$Wins), nrow(BaseballHits))
#BaseballHits$PredictedWinsEmpty <- predict(EmptyWinsModel)
BaseballHits$PredictedWinsEmpty <- rep(mean(BaseballHits$Wins), nrow(BaseballHits))

#residuals (deviations) = actual minus predicted values
BaseballHits$Wins - BaseballHits$PredictedWinsEmpty
#We can also use the resid() function
resid(EmptyWinsModel)
#Note the bizarre output format
BaseballHits$Residuals <- round(resid(EmptyWinsModel))
BaseballHits$Residuals <- BaseballHits$Wins - BaseballHits$PredictedWinsEmpty
#squared deviations = residuals ^ 2
BaseballHits$SquaredResiduals <- BaseballHits$Residuals^2
select(BaseballHits, Team, Wins, PredictedWinsEmpty, Residuals, SquaredResiduals)
#sum of squared deviations: use the sum() function
SumSquaredDeviations <- sum(BaseballHits$SquaredResiduals)
#We can use  anova() to figure out how much error there is around the model in terms of sum of squares
#Later in the course will we see how anova() can break down different sources of variance
anova(EmptyWinsModel)

#SSD easily influenced by sample size, so we can use variance as a more effective way to quantify error around the mean
#variance AKA "average squared deviation" AKA "mean squared error" 
#variance = s^2 = SSD/n-1
var(BaseballHits$Wins)
#standard deviation is the square root of the variance, and is measured in the same units as the original data being measured
sd(BaseballHits$Wins)
sqrt(var(BaseballHits$Wins))
favstats(BaseballHits$Wins)

# the z-score is a way to quantify variation of an observation using both the mean and standard deviation
# z = residual of observation/sd of sample = 𝑌𝑖−𝑌¯/s
BaseballHits$zWins <- zscore(BaseballHits$Wins)

#BaseballHits <- subset(BaseballHits, select = -c(Residuals:SquaredResiduals))
