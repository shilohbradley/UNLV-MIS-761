##################################################
##                                              ##
## Chapter 13                                   ##
## Repeated-measures designs (GLM 4)            ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("ez", "multcomp", "nlme", "pastecs", "reshape"))
# install.packages("WRS", repos = "http://R-Forge.R-project.org")
# install.packages(c("psycho", "lmer")) ## From Dr. Hardin's example

## Load packages -----
library(ez)
library(ggplot2)
library(here)
library(lmer)
library(multcomp)
library(nlme)
library(pastecs)
library(psycho) ## Package has been moved to "easystats"
library(reshape)
library(WRS)

##################################################
## ONE-WAY REPEATED-MEASURES ANOVA              ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Bushtucker.dat", header = TRUE)

## Manipulate data -----
df_long <- melt(df, id = "participant", 
                measured = c("stick_insect", "kangaroo_testicle", 
                             "fish_eye", "witchetty_grub"))
names(df_long) <- c("Participant", "Animal", "Retch")
df_long$Animal <- factor(df_long$Animal, 
                         labels = c("Stick Insect", "Kangaroo Testicle", 
                                    "Fish Eye", "Witchetty Grub"))
df_long <- df_long[order(df_long$Participant),]

## Explore the data -----
ggplot(df_long, aes(x = Animal, y = Retch)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  labs(x = "Type of Animal Eaten",
       y = "Mean Time to Retch (seconds)")  

by(df_long$Retch, df_long$Animal, stat.desc)

## Choosing contrasts -----
PartvsWhole <- c(1, -1, -1, 1)
TesticlevsEye <- c(0, -1, 1, 0)
StickvsGrub <- c(-1, 0, 0, 1)
contrasts(df_long$Animal) <- cbind(PartvsWhole, TesticlevsEye, StickvsGrub)

df_long$Animnal

## Build Repeated-measures ANOVA models -----
## ezANOVA() general form
## newModel <- ezANOVA(data = dataFrame, dv = .(outcome variable), 
##                     wid = .(variable that identifies participants), 
##                     within = .(repeated measures predictors), 
##                     between = .(between-group predictors), 
##                     detailed = FALSE, type = 2)

df_model <- ezANOVA(data = df_long, dv = .(Retch), wid = .(Participant), 
                    within = .(Animal), detailed = TRUE, type = 3)
df_model ## Sphericity is violated (great example of how arbitrary p = 0.05 is)

pairwise.t.test(df_long$Retch, df_long$Animal, paired = TRUE, 
                p.adjust.method = "bonferroni")

## Multilevel approach -----
## aov() general form:
## newModel <- aov(outcome ~ predictor, data = dataFrame)

## Can't use this because it assumes independence
# df_model <- aov(Retch ~ Animal, data = df_long) 

## lme() general form:
## newModel <- lme(outcome ~ predictor(s), random = random effects, 
##                 data = dataFrame, method = "ML")

df_model <- lme(Retch ~ Animal, random = ~1|Participant/Animal, 
                data = df_long, method = "ML")

baseline <- lme(Retch ~ 1, random = ~1|Participant/Animal, 
                data = df_long, method = "ML")

anova(baseline, df_model)

summary(df_model)

postHocs <- glht(df_model, linfct = mcp(Animal = "Tukey"))
summary(postHocs)
confint(postHocs)


## Robust one-way repeated-measures ANOVA -----
## Be skeptical of this section.
## Things may not be as accurate as we would expect since the packages
## can change over time and aren't always vetted.
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

## Dr. Hardin's tips for mixed models -----
summary(aov(Retch ~ Animal + Participant/Animal, data = df_long))
fit <- lmer(Retch ~ Animal + (1|Participant), data = df_long)
anova(fit)
print(fit)

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
df_long$imagery <- gl(3, 20, 180, labels = c("Positive", "Negative", "Neutral"))

## Explore the data ----
ggplot(df_long, aes(x = drink, y = attitue)) +
  geom_boxplot() +
  facet_wrap(~imagery, nrow = 1)

## Descriptive stats -----
options(digits = 3)
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
df_model <- ezANOVA(data = df_long, dv = .(attitude), 
                    wid = .(participant), within = .(drink, imagery), 
                    type = 3, detailed = TRUE)
df_model

pairwise.t.test(df_long$attitude, df_long$groups, paired = TRUE, 
                p.adjust.method = "bonferroni")

## Factorial repeated-measures designs as a GLM -----
baseline <- lme(attitude ~ 1, random = ~1|participant/drink/imagery, 
                data = df_long, method = "ML")
baseline <- lme(attitude ~ drink, random = ~1|participant/drink/imagery, 
                data = df_long, method = "ML")

drink_model <- update(baseline, .~. + drink)
imagery_model <- update(drink_model, .~. + imagery)
attitude_model <- update(imagery_model, .~. + drink:imagery)

anova(base_line, drink_model, imagery_model, attitude_model)

summary(attitude_model)



