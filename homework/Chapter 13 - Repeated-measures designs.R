##################################################
##                                              ##
## Chapter 13                                   ##
## Repeated-measures designs (GLM 4)            ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("ez", "multcomp", "nlme", "pastecs", "reshape"))
# install.packages("WRS", repos = "http://R-Forge.R-project.org")

## Load packages -----
library(ez)
library(ggplot2)
library(here)
library(multcomp)
library(nlme)
library(pastecs)
library(reshape)
library(WRS)

##################################################
## ONE-WAY REPEATED-MEASURES ANOVA              ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Bushtucker.dat", header = TRUE)

## Manipulate data -----
df_long <- melt(df, id = "participant", measured = c("stick_insect", "kangaroo_testicle", "fish_eye", "witchetty_grub"))
names(df_long) <- c("Participant", "Animal", "Retch")
df_long$Animal <- factor(df_long$Animal, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))
df_long <- df_long[order(df_long$Participant),]

## gl() general form:
## factor <- gl(number of levels, cases in each level, total cases, labels = c("label1", "label2", ...))

Participant <- gl(8, 4, labels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"))
Animal <- gl(4, 1, 32, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))
Retch <- c(8, 7, 1, 6, 9, 5, 2, 5, 6, 2, 3, 8, 5, 3, 1, 9, 8, 4, 5, 8, 7, 5, 6, 7, 10, 2, 7, 2, 12, 6, 8, 1)

df_long <- data.frame(Participant, Animal, Retch)

## Choosing contrasts -----
PartvsWhole <- c(1, -1, -1, 1)
TesticlevsEye <- c(0, -1, 1, 0)
StickvsGrub <- c(-1, 0, 0, 1)
contrasts(df_long$Animal) <- cbind(PartvsWhole, TesticlevsEye, StickvsGrub)

df_long$Animnal

## Build Repeated-measures ANOVA models -----
## ezANOVA() general form
## newModel <- ezANOVA(data = dataFrame, dv = .(outcome variable), wid = .(variable that identifies participants), 
##                     within = .(repeated measures predictors), between = .(between-group predictors), detailed = FALSE, type = 2)

df_model <- ezANOVA(data = df_long, dv = .(Retch), wid = .(Participant), within = .(Animal), detailed = TRUE, type = 3)
df_model

pairwise.t.test(df_long$Retch, df_long$Animal, paired = TRUE, p.adjust.method = "bonferroni")

## Multilevel approach -----
## General form:
## newModel <- aov(outcome ~ predictor, data = dataFrame)

df_model <- aov(Retch ~ Animal, data = df_long)

## lme() general form:
## newModel <- lme(outcome ~ predictor(s), random = random effects, data = dataFrame, method = "ML")

df_model <- lme(Retch ~ Animal, random = ~1|Participant/Animal, data = df_long, method = "ML")

baseline <- lme(Retch ~ 1, random = ~1|Participant/Animal, data = df_long, method = "ML")

anova(baseline, df_model)

summary(df_model)

postHocs <- glht(df_model, linfct = mcp(Animal = "Tukey"))
summary(postHocs)
confint(postHocs)


## Robust one-way repeated-measures ANOVA -----
dt <- df[, -c(1)]
dt

## rmanova() general form:
## rmanova(data, tr = 0.2)

rmanova(dt)

## rmanovab() general form:
## rmanovab(data, tr = 0.2, alpha = 0.05, nboot = 599)
rmanovab(dt, nboot = 2000)

## Post hoc tests based on a 20% trimmed mean
rmmcp(dt)

## Post hoc tests based on trimmed means and bootstrap
pairdepb(dt, nboot = 2000)

## Effect sizes for repeated-measures designs -----
rcontrast(3.149752, 21)
rcontrast(-0.101237, 21)
rcontrast(-1.923500, 21)

##################################################
## FACTORIAL REPEATED-MEASURES DESIGNS          ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Attitude.dat", header = TRUE)

## Manipulate data -----
df_long <- melt(attitudeData, id = "participant", 
                measured = c( "beerpos","beerneg", "beerneut", 
                              "winepos", "wineneg", "wineneut", 
                              "waterpos", "waterneg", "waterneut"))

names(df_long) <- c("participant", "groups", "attitude")

df_long$drink <- gl(3, 60, labels = c("Beer", "Wine", "Water"))
df_long$imagery <- gl(3,20, 180, labels = c("Positive", "Negative", "Neutral"))

by(df_long$attitude, list(df_long$drink, df_long$imagery), stat.desc, basic = FALSE)

## Choosing contrasts -----
AlcoholvsWater <- c(1, 1, -2)
BeervsWine <- c(-1, 1, 0)
contrasts(df_long$drink) <- cbind(AlcoholvsWater, BeervsWine)

NegativevsOther <- c(1, -2, 1)
PositivevsNeutral <- c(-1, 0, 1)
contrasts(df_long$imagery) <- cbind(NegativevsOther, PositivevsNeutral)

df_long$drink
df_long$imagery

## Build Repeated-measures ANOVA models -----
df_model <- ezANOVA(data = df_long, dv = .(attitude), wid = .(participant), within = .(drink, imagery), type = 3, detailed = TRUE)
df_model

pairwise.t.test(df_long$attitude, df_long$groups, paired = TRUE, p.adjust.method = "bonferroni")

## Factorial repeated-measures designs as a GLM
baseline <- lme(attitude ~ 1, random = ~1|participant/drink/imagery, data = df_long, method = "ML")
baseline <- lme(attitude ~ drink, random = ~1|participant/drink/imagery, data = df_long, method = "ML")

drink_model <- update(baseline, .~., + drink)
imagery_model <- update(drink_model, .~., + imagery)
attitude_model <- update(imagery_model, .~., + drink:imagery)

anova(base_line, drink_model, imagery_model, attitude_model)

summary(attitude_model)



