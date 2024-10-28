#Chapters 9-11 Review
#Alex Wasdahl
#29 Nov 21
#SOC SC 402
library(ggformula)
library(Lock5withR)
library(dplyr)
library(mosaic)
library(supernova)
#
#Chapter 9
#

#Creating a sampling distribution using resample() and rnorm()

#Let's start by simulating 100 dice rolls
#using the resample() function returns a vector of results
hundred_rolls_sample <- resample(1:6, 100)
#Now let's plot the results on a histogram. Note any unevenness in the distribution
#This is called a SAMPLE distribution
gf_histogram(~ hundred_rolls_sample)
#Calculate the mean of the results: note the difference between your sample mean and the population mean 3.5
#(3.5 would be the mean of an infinite amount of dice rolls)
mean(hundred_rolls_sample)
#Now let's repeat this process 10 times and obtain 10 different sample means
#using the do() function in this manner returns a data frame of our results
ten_100roll_sample_means <- do(10) * mean(resample(1:6, 100))
ten_100roll_sample_means
#Let's look at how these sample means are distributed by plotting the results on a histogram
#This is called a SAMPLING distribution
gf_histogram(~ mean, data = ten_100roll_sample_means)

#in the dice example, we had only 6 possible outcomes (1:6) in our population
#And we used resample() to generate our outcomes
#But if we want to simulate a random sample where not all population outcomes are known, and we know or can approximate the mean and sd of the population, we should use rnorm()
#rnorm() generates individual observations from a Normal distribution given the mean and sd of a population
#rnorm() takes as arguments the # of observations and the population parameters

#Let's use a baseball example here
#Let's look at the avg length in minutes of 30 MLB games played in 2011 in the data set BaseballTimes
#First, remove any empty rows from the BaseballTimes data set
BaseballTimes <- na.omit(BaseballTimes)
#This 30-game sample will represent our hypothetical approximation of the population
#Let's imagine the true population of interest is all 162 MLB games played in 2011
times_hypothetical_pop <- BaseballTimes$Time
#Using the mean and sd of BaseballTimes$Time as a stand-in for the true population mean/sd, we can simulate hypothetical individual game times
single_predicted_time <- rnorm(1, mean = mean(times_hypothetical_pop), sd = sd(times_hypothetical_pop))
#If we create a sample in which we simulate 30 individual observations, we can compare the variation between our 30-game hypothetical sample and our 30-game approximated "population"
times_sample1 <- rnorm(30, mean = mean(times_hypothetical_pop), sd = sd(times_hypothetical_pop))
times_sample1_mean <- mean(times_sample1)
#If we take a bunch of these samples and plot their means, we can create a SAMPLING DISTRIBUTION much like we did with dice rolls in the earlier example
ten_30game_sample_means <- do(10) * mean(rnorm(30, mean = mean(times_hypothetical_pop), sd = sd(times_hypothetical_pop)))
gf_histogram(~ mean, data = ten_30game_sample_means)
#We can use the tally() function to examine how many of our sample means are greater than or less than a particular value
tally(~mean>=180, data=ten_30game_sample_means)

#Central Limit Theorem
#The CLT tells us some important characteristics about sampling distributions
#1. SHAPE: typically, sampling distributions are normal
#2. CENTER: Sampling distributinos coalesce around a real (see dice roll ex.) or hypothetical (see baseball ex.) population mean
#3. SPREAD: As sample size increases, standard error decreases


#
#Chapter 10
#

#Constructing a confidence interval for a population mean (empty model)

#Given a population mean, when we take a sample of a population, the sample mean will be different from that population mean (sampling error)
#Confidence intervals allow us to make a prediction with a particular % of confidence that the true mean is within a certain range based on our estimate of the true mean
#Let's return to the baseball example and look at our original sample of baseball times
times_hypothetical_pop
#Our estimate of the true population mean would be our sample mean
mean(times_hypothetical_pop)
#And our estimate of the true population sd would be our sample sd
sd(times_hypothetical_pop)
#The confidence interval can be calculated as follows
#Confidence Interval = (point estimate)  +/- (margin of error) 
#Margin of error is equal to (critical value)*(standard error)
#Our parameter estimate of interest in this example is the sample mean b_0
#Our confidence interval to find the true population mean will take our point estimate and 
#add/subtract a certain amount of standard errors from that point estimate based on what % conf. interval we want
#Critical value for 90% confidence interval: 1.645
#Critical value for 95% confidence interval: 1.96
#Critical value for 99% confidence interval: 2.58
#The standard error is calculated by dividing the sample SD by the sqrt of the sample size
#You've done this before in the Week 6 Tutorials
sem <- sd(times_hypothetical_pop)/sqrt(nrow(BaseballTimes))
#Now let's calculate the upper and lower bounds of a 95% confidence interval
ci95_upper <- mean(times_hypothetical_pop) + 1.96*sem
ci95_lower <- mean(times_hypothetical_pop) - 1.96*sem
#We can now say that, based on our 30 game sample, if all 2011 MLB game times were known, we are 95% confident that the true mean game time will be in this range
#Let's do the same for 90% and 99% confidence intervals
ci90_upper <- mean(times_hypothetical_pop) + 1.645*sem
ci90_lower <- mean(times_hypothetical_pop) - 1.645*sem
ci99_upper <- mean(times_hypothetical_pop) + 2.58*sem
ci99_lower <- mean(times_hypothetical_pop) - 2.58*sem
#Note that as we increase our degree of confidence, the interval becomes wider. A larger range of possible values allows us to be more confident that our estimate lies within that range.
#A simpler way to do this is to create an empty model with Time as the outcome variable 
timeempty <- lm(Time ~ NULL, data = BaseballTimes)
#And use the confint.default() function to calculate a confidence interval (default is 95%) based on the parameter(s) of the model
confint.default(timeempty)
#Compare these values to ci95_upper and ci95_lower

#Constructing a confidence interval for a difference between means (two-group model)

#Okay let's do a DIFFERENT BASEBALL EXAMPLE now (man I'm really branching out aren't I?)
#We're going to use the WinsPowerModel from a previous lab
#Refresher: This model describes the relationship between a team's categorization as a PowerTeam (hitting above avg amt of home runs) impact on outcome variable Wins.
BaseballHits$PowerTeam <- BaseballHits$HomeRuns > median(BaseballHits$HomeRuns)
WinsPowerModel <- lm(Wins ~ PowerTeam, data = BaseballHits)
WinsPowerModel
#The parameter we are interested in now is b_1, the difference in Home Runs bw the PowerTeamTrue and PowerTeamFalse groups
#We want to see if the difference bw groups is real or just due to sampling variaton
#In this sense, we are comparing our 2-group model to the empty model in evaluating the usefulness of our difference bw groups statistic
#So we will construct a confidence interval within which the true difference is likely to lie
#If our confidence interval contains the value 0, then it is potentially likely that there is no real difference between groups, and the difference observed is merely due to sampling variation
RefGroupMean <- mean(BaseballHits$Wins[BaseballHits$PowerTeam == FALSE])
Group2Mean <- mean(BaseballHits$Wins[BaseballHits$PowerTeam == TRUE])
#b_1 = difference between group means = PowerTeamTRUE (secondary) group mean minus PowerTeamFALSE (reference) group mean
DiffBwGroups <- Group2Mean - RefGroupMean
#Can also be calculated using b1() function
b1(Wins ~ PowerTeam, data = BaseballHits)
#Confidence Interval = (point estimate)  +/-  (margin of error)
#MoE=(critical value)*(standard error)
#DiffBwGroups is our point estimate
#The critical value is the t-statistic, since we are dealing with the difference between groups.
#The standard error = sqrt((pooled variance/n1) + (pooled variance/n2)) where n1 and n2 are the sizes of each group
#THE BOOK DOESNT REALLY GET INTO THE STANDARD ERROR MENTIONED ABOVE. 
#THE BOOK HAS YOU SIMULATE SAMPLING DISTRIBUTIONS OF THE DIFFERENCE (b_1)
#Using the resample() function, arranging in descending order, and finding the 25th and 975th b_1 values 
#These values are used for the upper and lower bounds of a 95% confidence interval
#bootSDob1 stands for bootsrap sampling distribution of b1
bootSDob1 <- do(1000) * b1(Wins ~ PowerTeam, data = resample(BaseballHits, 30))
bootSDob1 <- arrange(bootSDob1, desc(b1))
bootSDob1$b1[25]
bootSDob1$b1[975]
#As earlier, we can also just use confint() on the two-group model
#This will report confidence intervals for both b_0 and b_1 estimates
confint(WinsPowerModel)
#The 2 important things to know here:
#The margin of error is based on variance of both groups as well as the sample size of each group.
#The method of simulating sample distributions rather than calculating it with the (point estimate)  +/-  (margin of error) formula or the confint() function is called BOOTSTRAPPING 

#Constructing a confidence interval for the slope of a regression line

#Let's use the baseball example again, but with Home Runs as a continuous variable rather than PowerTeam as a categorical variable
WinsHomersModel <- lm(Wins ~ HomeRuns, data = BaseballHits)
#When we construct confidence intervals for regression model parameter estimates, we still want b_1
#But this time b_1 represents the slope of the regression line rather than the difference between group means
b1(WinsHomersModel)
#b_1 is the incremental change in Y per unit X
#Again, we are comparing a "complex" model with the empty model
#A b_1 value of 0 would imply that X has no effect on Y
#Thus, if our confidence interval based on the b_1 values of simulated samples contained 0, then it would be
#entirely possible that the population b_1 = 0 and there would be no need to include X as an explanatory variable
#We can either do this mathematically using the confint() function
confint(WinsHomersModel)
#Or by bootstrapping (simulating 1000 samples and finding the upper and lower 2.5%, as done in the book)
bootSDob1_2 <- do(1000) * b1(Wins ~ HomeRuns, data = resample(BaseballHits, 30))
bootSDob1_2 <- arrange(bootSDob1_2, desc(b1))
bootSDob1_2$b1[25]
bootSDob1_2$b1[975]

#
#Chapter 11
#

#Approximating p-values based on PRE and F values

#Let's revisit PRE and F values by looking at the supernova() output for our WinsPowerModel
supernova(WinsPowerModel)
#F ratio and PRE are both sample statistics that give us a sense of how much error is explained by our complex (2-group) model relative to the empty model
samplePRE <- PRE(WinsPowerModel)
sampleF <- fVal(WinsPowerModel)
#These statistics are based on the 30 teams in the BaseballHits data set, but if we took a different sample of teams, both PRE and F would differ bw samples, just like sample means and differences bw group means as we saw already
#Let's figure out the likelihood that our sample PRE or sample F was generated by the empty model
#In other words, let's figure out the chance of achieving our observed PRE and F values if the DGP we are imagining possesses no relationship between Wins and PowerTeam
#We can use the shuffle() function to "shuffle" the explanatory variable, essentially creating a randomized relationship between outcome and explanatory variable
lm(Wins ~ shuffle(PowerTeam), data = BaseballHits)
#Let's now simulate 1000 PREs based on this shuffled model in which differences between groups are due to randomness
ThousandPREs <- do(1000) * PRE(Wins ~ shuffle(PowerTeam), data = BaseballHits)
tally(~PRE > samplePRE, data = ThousandPREs, format = "proportion")
#Let's now do the same thing with F ratios
ThousandFs <- do(1000) * fVal(Wins ~ shuffle(PowerTeam), data = BaseballHits)
tally(~fVal > sampleF, data=ThousandFs, format = "proportion")
#Both of these values serve as approximations of the p-value in the supernova() output
#The p value is the likelihood that a value more extreme than our sample statistic would be generated from the empty model
#In other words, the p value tells us the likelihood that our sample PRE or sample F was generated by the empty model

# Type I and II error

# When looking at constructing confidence intervals for difference bw means and linear regression models, we compare our complex model to the empty model
# In a practical sense, we make decisions to adopt models based on our data
# The null hypothesisis the proposition that there is no effect or no relationship between phenomena or populations
# In this case it would be to conclude that our complex models offered no significant difference from the empty model
# Adopting the complex model would mean that we are rejecting the null hypothesis of no difference
# There are four scenarios that are important for understanding Type I and II error
# Empty Model is true and we adopt Empty Model (fail to reject null hypothesis): Yay us!
# Empty Model is true and we adopt Complex Model (reject null hypothesis): Type I Error
# Complex Model is true and we adopt Empty Model (fail to reject null hypothesis): Type II Error
# Complex Model is true and we adopt Complex Model (reject null hypothesis): YAY US!