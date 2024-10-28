#Lab 11/12/20
#TA: Alex Wasdahl

#Load the following packages
library(ggformula)
library(Lock5withR)
library(dplyr)
library(mosaic)
library(supernova)

#Last time we looked at the empty model, which featured a single outcome variable in which we "explained" its variation 
#by looking at its mean. The mean was our best shot at explaining variation because it minimizes the SSD (sum of squared dev's)
EmptyWinsModel <- lm(Wins ~ NULL, data = BaseballHits)
EmptyWinsModel
anova(EmptyWinsModel)

#Let's next look at adding a categorical explanatory variable to our model
#We're looking at explaining variation in Wins in our MLB data set - perhaps home runs could serve as a useful explanatory variable
select(BaseballHits, Team, Wins, HomeRuns)

#Let's convert home runs into a categorical variable by creating a PowerTeam variable that returns true when the team hit an above-median
#number of home runs and false when the team hit a below-median number of home runs
BaseballHits$PowerTeam <- BaseballHits$HomeRuns > median(BaseballHits$HomeRuns)
BaseballHits$PowerTeam

#Let's look at the distribution of Wins between the two PowerTeam categories
gf_point(Wins ~ PowerTeam, data = BaseballHits) %>%
  gf_hline(yintercept = ~ mean(Wins), color = "gray") %>%
  gf_hline(yintercept = ~ mean(Wins[PowerTeam == FALSE]), color = "magenta") %>%
  gf_hline(yintercept = ~ mean(Wins[PowerTeam == TRUE]), color = "turquoise")
#gray line: grand mean (mean wins for all teams aka parameter estimate for empty model)
#magenta line: group mean for Non-PowerTeams
#turquoise line: group mean for PowerTeams

#Going back to the fundamental equation DATA = MODEL + ERROR, error is still measured the same way, as the deviation 
#of each observed value from its predicted value. But this time, the predicted value is the group mean (PowerTeam = False or PowerTeam = True) 
#instead of from the Grand Mean (the mean of the outcome variable Wins)

#Now let's fit a linear model with the explanatory variable PowerTeam
WinsPowerModel <- lm(Wins ~ PowerTeam, data = BaseballHits)
WinsPowerModel

#The coefficients represent the estimates for b0 and b1 in the equation Yi=b0+b1Xi+ei
#Yi: Wins for observation (team) i
#Xi: PowerTeam value (False/True coded as 0/1) for observation (team) i 
#b0: mean value for PowerTeamFALSE teams
#b1: INCREMENTAL value ADDED to the mean value of the PowerTeamFALSE teams
# to get the mean value PowerTeamTRUE teams - aka the difference bw group means
#ei: each team's residual/error - difference between observed # of wins and predicted # of wins (group mean)
mean(BaseballHits$Wins[BaseballHits$PowerTeam == FALSE])
mean(BaseballHits$Wins[BaseballHits$PowerTeam == TRUE])
mean(BaseballHits$Wins[BaseballHits$PowerTeam == TRUE]) - mean(BaseballHits$Wins[BaseballHits$PowerTeam == FALSE])

#Let's look at the grand and group means as predictions of the empty and PowerTeam models, respectively
BaseballHits$Empty.predicted <- predict(EmptyWinsModel)
BaseballHits$Empty.residual <- round(resid(EmptyWinsModel))
BaseballHits$PowerTeam.predicted <- predict(WinsPowerModel)
BaseballHits$PowerTeam.residual <- resid(WinsPowerModel)
BaseballHits

#Next, let's compare the SSD (sum of squared deviations) bw the empty and group models
Empty.SSD <- sum(BaseballHits$Empty.residual^2)
PowerTeam.SSD <- sum(BaseballHits$PowerTeam.residual^2)
Empty.SSD
PowerTeam.SSD

#Notice that the SSD of the group model is less than that of the empty model. This is because the group model explains more variation.
#The Empty.SSD value represents the total variation in outcome (because nothing has been explained yet!) aka SSTotal
anova(EmptyWinsModel)
#The PowerTeam.SSD value represents the total variation remaining AFTER the addition of our explanatory variable
anova(WinsPowerModel)
#We can combine the information from these two anova tables using the supernova() function
supernova(WinsPowerModel)

#Total(empty model) is the total variation in the outcome variable (SS Total)
SSTotal <- Empty.SSD
SSTotal
#Error(from model) is the unexplained error remaining (SS Error)
SSError <- PowerTeam.SSD
SSError
#Model(error reduced) is the error explained away by the group model (SS Model)
SSModel <- Empty.SSD - PowerTeam.SSD
SSModel
#PRE = SSModel/SSTotal = (SSTotal - SSError / SSTotal)
#PRE represents the proportional effectiveness of our model, how much of our total variation has been explained away
PRE <- SSModel/SSTotal
PRE

#Let's now look at how splitting the teams into three power categories rather than two impacts model effectiveness
Power3Groups <- ntile(BaseballHits$HomeRuns, 3)
BaseballHits$PowerTeam3Groups <- factor(Power3Groups, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
BaseballHits$PowerTeam3Groups

#This distribution shows us that our observations are compared to their respective group means
gf_point(Wins ~ PowerTeam3Groups, data = BaseballHits) %>%
  gf_hline(yintercept = ~ mean(Wins), color = "gray") %>%
  gf_hline(yintercept = ~ mean(Wins[PowerTeam3Groups == "low"]), color = "magenta") %>%
  gf_hline(yintercept = ~ mean(Wins[PowerTeam3Groups == "medium"]), color = "yellow") %>%
  gf_hline(yintercept = ~ mean(Wins[PowerTeam3Groups == "high"]), color = "turquoise")

#Create the three-group linear model
WinsPower3GroupsModel <- lm(Wins ~ PowerTeam3Groups, data = BaseballHits)
WinsPower3GroupsModel

#Note that PowerTeam3Groupsmedium and PowerTeam3Groupshigh are incremental differences - BOTH relative to PowerTeam3Groupslow
mean(BaseballHits$Wins[BaseballHits$PowerTeam3Groups == "low"])
mean(BaseballHits$Wins[BaseballHits$PowerTeam3Groups == "medium"])
mean(BaseballHits$Wins[BaseballHits$PowerTeam3Groups == "high"])

#Now let's look at the effectiveness of our model using supernova()
supernova(WinsPower3GroupsModel)

#The PRE is higher, which means we explained away more variation, but we don't know 
#how much error has been reduced relative to the complexity we've added to the model (by estimating another parameter).
#So in this sense, PRE isn't actually very proportional... but thankfully, we have the F Ratio!
#F-Ratio: ratio of variance explained to variance unexplained
#F = MSModel/MSError = (SSModel/dfModel)/(SSError/dfError)
#Compare this to PRE = SSModel/SSTotal: ratio of error explained to total error
#Larger F ratio means a stronger effect of the model

#Let's look at this further by examining degrees of freedom and mean squared error. 
#Degrees of freedom (or df) can be thought of like a currency
#you “spend” a degree of freedom for each parameter estimate you include in a model
#Empty model spends one degree of freedom estimating Grand Mean parameter
supernova(EmptyWinsModel)
#2-group model spends two degrees of freedom estimating two Group Mean parameters
supernova(WinsPowerModel)
#3-group model spends three degrees of freedom estimating three Group Mean parameters
supernova(WinsPower3GroupsModel)

#Mean squared error of empty model = MSTotal = SSTotal/dfTotal = SSD/n-1
#Mean squared error reduced = MSModel = SSModel/dfModel
#Mean squared error remaining = MSError = SSError/dfError 

BaseballHits$RunsTeam <- BaseballHits$Runs > median(BaseballHits$Runs)
WinsRunsModel <- lm(Wins ~ RunsTeam, data = BaseballHits)
WinsRunsModel
supernova(WinsRunsModel)
BaseballHits$RunsTeam3Cats <- factor(ntile(BaseballHits$Runs, 3), levels = c(1, 2, 3), labels = c("low", "medium", "high"))
WinsRuns3CatsModel <- lm(Wins ~ RunsTeam3Cats, data=BaseballHits)
supernova(WinsRuns3CatsModel)
