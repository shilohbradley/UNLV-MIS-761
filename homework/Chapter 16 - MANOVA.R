##################################################
##                                              ##
## Chapter 16                                   ##
## Multivariate analysis of variance (MANOVA)   ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("mvoutlier", "mvnormtest"))

## Load packages -----
library(car)
library(ggplot2)
library(MASS)
library(mvoutlier)
library(mvnormtest)
library(pastecs)
library(reshape)
library(WRS)

## Function definitions -----

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/OCD.dat", header = TRUE)

## Manipulate the data -----
df$Group <- factor(df$Group, levels = c("CBT", "BT", "No Treatment Control", labels = c("CBT", "BT", "NT")))

## Exploring the data -----
ggplot(df, aes(x = Actions, y = Thoughts)) +
  geom_smooth(method = "glm") +
  geom_point() +
  facet_grid(. ~ Group) +
  labs(x = "Number of Obsession-Related Behaviors",
       y = "Number of Obsession-Related Thoughts")

df %>%
  melt(id = "Group", measured = c("Action", "Thoughts")) %>%
  ggplot(aes(x = Group, y = value, fill = variable)) +
    geom_boxplot()

by(df$Actions, df$Group, stat.desc, basic = FALSE)
by(df$Thoughts, df$Group, stat.desc, basic = FALSE)
by(df[, 2:3], df$Group, cov)

cbt <- df[1:10, 2:3]
cbt <- t(cbt)

bt <- t(df[11:20, 2:3])
nt <- t(df[21:30, 2:3])

mshapiro.test(cbt)
mshapiro.test(bt)
mshapiro.test(nt)

aq.plot(df[, 2:3])

## Setting the contrasts -----
contrasts(df$Group) <- contr.treatment(3, base = 3)

CBT_vs_NT <- c(1, 0, 0)
BT_vs_NT <- c(0, 1, 0)
contrasts(df$Group) <- cbind(CBT_vs_NT, BT_vs_NT)

##################################################
## MANOVA MODEL                                 ##
##################################################

## Build the model -----
## manova() general form
## newModel <- manova(outcome ~ predictor(s), data = dataFrame, na.action = an action)

outcome <- cbind(df$Actions, df$Thoughts)

ocd_model <- manova(outcome ~ Group, data = df)

## Analyze the model -----
summary(ocd_model, intercept = TRUE)
summary(ocd_model, intercept = TRUE, test = "Wilks")
summary(ocd_model, intercept = TRUE, test = "Hotelling")
summary(ocd_model, intercept = TRUE, test = "Roy")
summary.aov(ocd_model)

## Setting the contrasts -----
action_model <- lm(Actions ~ Group, data = df)
thoughts_model <- lm(Thoughts ~ Group, data = df)

summary.lm(action_model)
summary.lm(thoughts_model)

## Robust MANOVA -----
df$row <- rep(1:10, 3)

df_melt <- melt(df, id = c("Group", "row"), measured = c("Actions", "Thoughts"))
names(df_melt) <- c("Group", "row", "Outcome_Measure", "Frequency")

df_robust <- cast(df_melt, row ~ Group + Outcome_Measure, value = "Frequency")
df_robust$row <- NULL
df_robust

## mulrank() and cmanova() general forms
## mulrank(number of groups, number of outcome measures, data)
## cmanova(number of groups, number of outcome measures, data)

mulrank(3, 2, df_robust)
cmanova(3, 2, df_robust)

## lda() general form
## newModel <- lda(Group ~ Predictor(s), data = dataFrame, prior = prior probabilities, na.action = "na.omit")

lda_model <- lda(Group ~ Actions + Thoughts, data = df)
lda_model

predict(lda_model)
plot(lda_model)