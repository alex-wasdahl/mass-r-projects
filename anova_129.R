library(dplyr)
library(psych)
library(mosaic)
library(supernova)
library(jtools)
library(multcomp)
library(ggplot2)
#import data set
res <- read.csv("FRSClean.csv", header = T)

#add subject id column, allowing R to recognize which observations are repeated measures from the same subject
res$id <- rep(1:nrow(res))

#split dataset into two groups of respondents: those who saw the AI-generated articles and those who saw the human-generated articles
res_AI <- na.omit(dplyr::select(res, "AI.Finance.Credibility":"AI.Tech.Neutrality", "id"))
res_Human <- na.omit(dplyr::select(res, "Human.Finance.Credibility":"Human.Tech.Neutrality", "id"))

#split responses between the three article topics for the AI group
res_AI_Finance <- dplyr::select(res_AI, "AI.Finance.Credibility":"AI.Finance.Neutrality", "id")
res_AI_Politics <- dplyr::select(res_AI, "AI.Politics.Credibility":"AI.Politics.Neutrality", "id")
res_AI_Tech <- dplyr::select(res_AI, "AI.Tech.Credibility":"AI.Tech.Neutrality", "id")

#split responses between the three article topics for the Human group
res_Human_Finance <- dplyr::select(res_Human, "Human.Finance.Credibility":"Human.Finance.Neutrality", "id")
res_Human_Politics <- dplyr::select(res_Human, "Human.Politics.Credibility":"Human.Politics.Neutrality", "id")
res_Human_Tech <- dplyr::select(res_Human, "Human.Tech.Credibility":"Human.Tech.Neutrality", "id")

#create a topic variable to assess differences between articles within each group

#rename variables to merge across topics e.g. AI.Finance.Credibility, AI.Politics.Credibility, and AI.Tech.Credibility --> 1 (credibility)
names(res_AI_Finance) <- c("trust", "prolixity", "engagement", "substance", "clarity", "alienation", "complexity", "effort", "neutrality", "id")
names(res_AI_Politics) <- c("trust", "prolixity", "engagement", "substance", "clarity", "alienation", "complexity", "effort", "neutrality", "id")
names(res_AI_Tech) <- c("trust", "prolixity", "engagement", "substance", "clarity", "alienation", "complexity", "effort", "neutrality", "id")

#create topic variable in AI group
res_AI_Finance$topic <- rep("Finance", nrow(res_AI_Finance))
res_AI_Politics$topic <- rep("Politics", nrow(res_AI_Politics))
res_AI_Tech$topic <- rep("Tech", nrow(res_AI_Tech))

#merge variables across topics
names(res_Human_Finance) <- c("trust", "prolixity", "engagement", "substance", "clarity", "alienation", "complexity", "effort", "neutrality", "id")
names(res_Human_Politics) <- c("trust", "prolixity", "engagement", "substance", "clarity", "alienation", "complexity", "effort", "neutrality", "id")
names(res_Human_Tech) <- c("trust", "prolixity", "engagement", "substance", "clarity", "alienation", "complexity", "effort", "neutrality", "id")

#topic variable in Human group
res_Human_Finance$topic <- rep("Finance", nrow(res_Human_Finance))
res_Human_Politics$topic <- rep("Politics", nrow(res_Human_Politics))
res_Human_Tech$topic <- rep("Tech", nrow(res_Human_Tech))

#create merged authorship group data sets with topic variable
res_AI_merged <- rbind(res_AI_Finance, res_AI_Politics, res_AI_Tech)
res_Human_merged <- rbind(res_Human_Finance, res_Human_Politics, res_Human_Tech)

#create fully merged data set with authorship variable
res_AI_merged$authorship <- rep("AI", nrow(res_AI_merged))
res_Human_merged$authorship <- rep("Human", nrow(res_Human_merged))
res_all <- rbind(res_AI_merged, res_Human_merged)

res_finance <- filter(res_all, topic == "Finance")
res_politics <- filter(res_all, topic == "Politics")
res_tech <- filter(res_all, topic == "Tech")

#Two way repeated measures ANOVAs
#plot(trust ~ factor(authorship) + factor(topic), data = res_all)

TrustTwoWay <- aov(trust ~ authorship * topic + Error(id/topic), data = res_all)
ProlTwoWay <- aov(prolixity ~ authorship * topic + Error(id/topic), data = res_all)
EngTwoWay <- aov(engagement ~ authorship * topic + Error(id/topic), data = res_all)
SubsTwoWay <- aov(substance ~ authorship * topic + Error(id/topic), data = res_all)
ClarTwoWay <- aov(clarity ~ authorship * topic + Error(id/topic), data = res_all)
AlienTwoWay <- aov(alienation ~ authorship * topic + Error(id/topic), data = res_all)
CompTwoWay <- aov(complexity ~ authorship * topic + Error(id/topic), data = res_all)
EffTwoWay <- aov(effort ~ authorship * topic + Error(id/topic), data = res_all)
NeutTwoWay <- aov(neutrality ~ authorship * topic + Error(id/topic), data = res_all)

summary(aov(trust ~ topic + Error(id/topic), data = res_all))
trustauthorship <- aov(trust ~ authorship, data = res_all)
prolauthorship <- aov(prolixity ~ authorship, data = res_all)
engauthorship <- aov(engagement ~ authorship, data = res_all)
subsauthorship <- aov(substance ~ authorship, data = res_all)
clarauthorship <- aov(clarity ~ authorship, data = res_all)
alienauthorship <- aov(alienation ~ authorship, data = res_all)
compauthorship <- aov(complexity ~ authorship, data = res_all)
effauthorship <- aov(effort ~ authorship, data = res_all)
neutauthorship <- aov(neutrality ~ authorship, data = res_all)
summary(trustauthorship)
summary(prolauthorship)
summary(engauthorship)
summary(subsauthorship)
summary(clarauthorship)
summary(alienauthorship)
summary(compauthorship)
summary(effauthorship)
summary(neutauthorship)

summary(TrustTwoWay)
#no sig

summary(ProlTwoWay)
#significant main effect of authorship
lm(res_all$prolixity~ res_all$authorship)

summary(EngTwoWay)
#significant main effect of authorship and interaction effect
lm(res_all$engagement ~ res_all$authorship)
#plot the mean engagement ratings for each combination of authorship and topic for this var
ggplot(res_all, aes(x = topic, y = engagement, fill = authorship)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 0.9), width = 0.8) +
  labs(title = "Engagement Ratings by Authorship and Topic", x = "Topic", y = "Engagement") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

summary(SubsTwoWay)
#no sig

summary(ClarTwoWay)
#significant main effect of authorship and interaction effect
lm(res_all$clarity ~ res_all$authorship)
#plot the mean engagement ratings for each combination of authorship and topic for this var
ggplot(res_all, aes(x = topic, y = clarity, fill = authorship)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 0.9), width = 0.8) +
  labs(title = "Clarity Ratings by Authorship and Topic", x = "Topic", y = "Clarity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

summary(AlienTwoWay)
#no sig

summary(CompTwoWay)
#significant interaction effect

summary(EffTwoWay)
#significant interaction effect

summary(NeutTwoWay)
#significant main effect of authorship
lm(res_all$neutrality ~ res_all$authorship)

# Assuming your data frame is named 'data' and includes columns 'authorship', 'topic', and 'rating'
mean_ratings <- aggregate(cbind(trust, prolixity, engagement, substance, clarity, alienation, complexity, effort, neutrality) ~ authorship + topic, data = res_all, FUN = mean)

library(dplyr)
library(knitr)

# Trust table
trust_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(trust), .groups = 'drop')
wide_trust_data <- spread(trust_data, key = topic, value = mean_rating)
kable(wide_trust_data, caption = "Mean Trust Ratings by Authorship and Topic")

# Prolixity table
prolixity_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(prolixity), .groups = 'drop')
wide_prolixity_data <- spread(prolixity_data, key = topic, value = mean_rating)
kable(wide_prolixity_data, caption = "Mean Prolixity Ratings by Authorship and Topic")

# Engagement table
engagement_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(engagement), .groups = 'drop')
wide_engagement_data <- spread(engagement_data, key = topic, value = mean_rating)
kable(wide_engagement_data, caption = "Mean Engagement Ratings by Authorship and Topic")

# Substance table
substance_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(substance), .groups = 'drop')
wide_substance_data <- spread(substance_data, key = topic, value = mean_rating)
kable(wide_substance_data, caption = "Mean Substance Ratings by Authorship and Topic")

# Clarity table
clarity_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(clarity), .groups = 'drop')
wide_clarity_data <- spread(clarity_data, key = topic, value = mean_rating)
kable(wide_clarity_data, caption = "Mean Clarity Ratings by Authorship and Topic")

# Alienation table
alienation_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(alienation), .groups = 'drop')
wide_alienation_data <- spread(alienation_data, key = topic, value = mean_rating)
kable(wide_alienation_data, caption = "Mean Alienation Ratings by Authorship and Topic")

# Complexity table
complexity_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(complexity), .groups = 'drop')
wide_complexity_data <- spread(complexity_data, key = topic, value = mean_rating)
kable(wide_complexity_data, caption = "Mean Complexity Ratings by Authorship and Topic")

# effort table
effort_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(effort), .groups = 'drop')
wide_effort_data <- spread(effort_data, key = topic, value = mean_rating)
kable(wide_effort_data, caption = "Mean Effort Ratings by Authorship and Topic")

# neutrality table
neutrality_data <- res_all %>%
  group_by(authorship, topic) %>%
  summarise(mean_rating = mean(neutrality), .groups = 'drop')
wide_neutrality_data <- spread(neutrality_data, key = topic, value = mean_rating)
kable(wide_neutrality_data, caption = "Mean Neutrality Ratings by Authorship and Topic")

results <- res_all %>%
  group_by((cbind(authorship, topic))) %>%
  summarise(
    mean = mean(alienation),
    sd = sd(alienation),
    count = n(),
    sem = sd / sqrt(count)
  )
#create AI group regression models with X = article topic and Y = factor variable

lm1 <- lm(trust ~ authorship * topic, data = res_all)
lm2 <- lm(prolixity ~ authorship * topic, data = res_all)
lm3 <- lm(engagement ~ authorship * topic, data = res_all)
lm4 <- lm(substance ~ authorship * topic, data = res_all)
lm5 <- lm(clarity ~ authorship * topic, data = res_all)
lm6 <- lm(alienation ~ authorship * topic, data = res_all)
lm7 <- lm(complexity ~ authorship * topic, data = res_all)
lm8 <- lm(effort ~ authorship * topic, data = res_all)
lm9 <- lm(neutrality ~ authorship * topic, data = res_all)

lm1 <- lm(trust ~ topic, data = res_all)
lm2 <- lm(prolixity ~ topic, data = res_all)
lm3 <- lm(engagement ~ topic, data = res_all)
lm4 <- lm(substance ~ topic, data = res_all)
lm5 <- lm(clarity ~ topic, data = res_all)
lm6 <- lm(alienation ~ topic, data = res_all)
lm7 <- lm(complexity ~ topic, data = res_all)
lm8 <- lm(effort ~ topic, data = res_all)
lm9 <- lm(neutrality ~ topic, data = res_all)

lm1 <- lm(trust ~ authorship:topic, data = res_all)
lm2 <- lm(prolixity ~ authorship:topic, data = res_all)
lm3 <- lm(engagement ~ authorship:topic, data = res_all)
lm4 <- lm(substance ~ authorship:topic, data = res_all)
lm5 <- lm(clarity ~ authorship:topic, data = res_all)
lm6 <- lm(alienation ~ authorship:topic, data = res_all)
lm7 <- lm(complexity ~ authorship:topic, data = res_all)
lm8 <- lm(effort ~ authorship:topic, data = res_all)
lm9 <- lm(neutrality ~ authorship:topic, data = res_all)

plot_summs(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9, model.names = c("Trust", "Prolixity", "Engagement", "Substance", "Clarity", "Alienation", "Complexity", "Effort", "Neutrality"), colors = "Paired")
#interaction plots for complexity, effort, clarity, and engagement
interaction.plot(
  x.factor = res_all$authorship,
  trace.factor = res_all$topic,
  response = res_all$complexity,
  fun = mean,
  ylab = "Complexity",
  xlab = "Authorship",
  trace.label = "Topic")

TrustAuthorshipFinance <- lm(res_finance$trust ~ res_finance$authorship)
ProlAuthorshipFinance <- lm(res_finance$prolixity ~ res_finance$authorship)
EngAuthorshipFinance <- lm(res_finance$engagement ~ res_finance$authorship)
SubsAuthorshipFinance <- lm(res_finance$substance ~ res_finance$authorship)
ClarAuthorshipFinance <- lm(res_finance$clarity ~ res_finance$authorship)
AlienAuthorshipFinance <- lm(res_finance$alienation ~ res_finance$authorship)
CompAuthorshipFinance <- lm(res_finance$complexity ~ res_finance$authorship)
EffAuthorshipFinance <- lm(res_finance$effort ~ res_finance$authorship)
NeutAuthorshipFinance <- lm(res_finance$neutrality ~ res_finance$authorship)
TrustAuthorshipPolitics <- lm(res_politics$trust ~ res_politics$authorship)
ProlAuthorshipPolitics <- lm(res_politics$prolixity ~ res_politics$authorship)
EngAuthorshipPolitics <- lm(res_politics$engagement ~ res_politics$authorship)
SubsAuthorshipPolitics <- lm(res_politics$substance ~ res_politics$authorship)
ClarAuthorshipPolitics <- lm(res_politics$clarity ~ res_politics$authorship)
AlienAuthorshipPolitics <- lm(res_politics$alienation ~ res_politics$authorship)
CompAuthorshipPolitics <- lm(res_politics$complexity ~ res_politics$authorship)
EffAuthorshipPolitics <- lm(res_politics$effort ~ res_politics$authorship)
NeutAuthorshipPolitics <- lm(res_politics$neutrality ~ res_politics$authorship)
TrustAuthorshipTech <- lm(res_tech$trust ~ res_tech$authorship)
ProlAuthorshipTech <- lm(res_tech$prolixity ~ res_tech$authorship)
EngAuthorshipTech <- lm(res_tech$engagement ~ res_tech$authorship)
SubsAuthorshipTech <- lm(res_tech$substance ~ res_tech$authorship)
ClarAuthorshipTech <- lm(res_tech$clarity ~ res_tech$authorship)
AlienAuthorshipTech <- lm(res_tech$alienation ~ res_tech$authorship)
CompAuthorshipTech <- lm(res_tech$complexity ~ res_tech$authorship)
EffAuthorshipTech <- lm(res_tech$effort ~ res_tech$authorship)
NeutAuthorshipTech <- lm(res_tech$neutrality ~ res_tech$authorship)

summary(TrustAuthorshipFinance)
summary(ProlAuthorshipFinance)
summary(EngAuthorshipFinance)
summary(SubsAuthorshipFinance)
summary(ClarAuthorshipFinance)
summary(AlienAuthorshipFinance)
summary(CompAuthorshipFinance)
summary(EffAuthorshipFinance)
summary(NeutAuthorshipFinance)

plot_summs(TrustAuthorshipFinance, ProlAuthorshipFinance, EngAuthorshipFinance, SubsAuthorshipFinance, ClarAuthorshipFinance, AlienAuthorshipFinance, CompAuthorshipFinance, EffAuthorshipFinance, NeutAuthorshipFinance, model.names = c("Trust", "Prolixity", "Engagement", "Substance", "Clarity", "Alienation", "Complexity", "Effort", "Neutrality"), colors = "Paired")


summary(TrustAuthorshipPolitics)
summary(ProlAuthorshipPolitics)
summary(EngAuthorshipPolitics)
summary(SubsAuthorshipPolitics)
summary(ClarAuthorshipPolitics)
summary(AlienAuthorshipPolitics)
summary(CompAuthorshipPolitics)
summary(EffAuthorshipPolitics)
summary(NeutAuthorshipPolitics)

plot_summs(TrustAuthorshipPolitics, ProlAuthorshipPolitics, EngAuthorshipPolitics, SubsAuthorshipPolitics, ClarAuthorshipPolitics, AlienAuthorshipPolitics, CompAuthorshipPolitics, EffAuthorshipPolitics, NeutAuthorshipPolitics, model.names = c("Trust", "Prolixity", "Engagement", "Substance", "Clarity", "Alienation", "Complexity", "Effort", "Neutrality"), colors = "Paired")

summary(TrustAuthorshipTech)
summary(ProlAuthorshipTech)
summary(EngAuthorshipTech)
summary(SubsAuthorshipTech)
summary(ClarAuthorshipTech)
summary(AlienAuthorshipTech)
summary(CompAuthorshipTech)
summary(EffAuthorshipTech)
summary(NeutAuthorshipTech)

plot_summs(TrustAuthorshipTech, ProlAuthorshipTech, EngAuthorshipTech, SubsAuthorshipTech, ClarAuthorshipTech, AlienAuthorshipTech, CompAuthorshipTech, EffAuthorshipTech, NeutAuthorshipTech, model.names = c("Trust", "Prolixity", "Engagement", "Substance", "Clarity", "Alienation", "Complexity", "Effort", "Neutrality"), colors = "Paired")



