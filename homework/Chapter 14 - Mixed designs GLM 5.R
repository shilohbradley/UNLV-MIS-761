##################################################
##                                              ##
## Chapter 14                                   ##
## Mixed designs (GLM 5)                        ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("ez", "multcomp", "nlme", "pastecs", "reshape"))
# install.packages("WRS", repos = "http://R-Forge.R-project.org")

## Load packages -----
library(compute.es)
library(ez)
library(ggplot2)
library(multcomp)
library(nlme)
library(pastecs)
library(reshape)
library(WRS)
source("http://www-rcf.usc.edu/~rwilcox/Rallfun-v14")

## Function definitions -----
rcontrast <- function(t, df) {
  
    r <- sqrt(t ^ 2 / (t ^ 2 + df))
    print(paste("r = ", r))
}

##################################################
## MIXED DESIGNS                                ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/LooksOrPersonality.dat", header = TRUE)

## Manipulate data -----
df_speed <- melt(df, id = c("participant","gender"), 
                 measured = c("att_high", "av_high", "ug_high", 
                              "att_some", "av_some", "ug_some", 
                              "att_none", "av_none", "ug_none"))

names(df_speed) <- c("participant", "gender", "groups", "dateRating")

df_speed$personality <- gl(3, 60, labels = c("Charismatic", "Average", "Dullard"))
df_speed$looks <- gl(3, 20, 180, labels = c("Attractive", "Average", "Ugly"))

df_speed <- df_speed[order(df_speed$participant), ]

by(speedData$dateRating, list(speedData$looks, speedData$personality), 
   stat.desc, basic = FALSE)

## Choosing contrasts -----
SomevsNone <- c(1, 1, -2)
HivsAv <- c(1, -1, 0)
contrasts(df_speed$personality) <- cbind(SomevsNone, HivsAv)

AttractivevsUgly <- c(1, 1, -2)
AttractvsAv <- c(1, -1, 0)
contrasts(df_speed$looks) <- cbind(AttractivevsUgly, AttractvsAv)

## Build mixed ANOVA model -----
speed_model <- ezANOVA(data = df_speed, dv = .(dateRating), wid = .(participant),
                       between = .(gender), within = .(looks, personality),
                       type = 3, detailed = TRUE)
speed_model

## Choosing contrasts -----
## Use same contrasts a mixed ANOVA model, but change this one
HighvsAv <- c(1, 0, 0)
DullvsAv <- c(0, 0, 1)
contrasts(df_speed$personality) <- cbind(HighvsAv, DullvsAv)

df_speed$looks
df_speed$personality

## Build Mixed designs as a GLM model -----
baseline <- lme(dateRating ~ 1, random = ~1|participant/looks/personality,
                data = df_speed, method = "ML")

looks_model <- lme(dateRating ~ looks, random = ~1|participant/looks/personality, 
                   data = df_speed, method = "ML")
looks_model <- update(baseline, .~. + looks)

personality_model <- update(looks_model, .~., + personality)

gender_model <- update(personality_model, .~., + gender)

looks_gender <- update(gender_model, .~. + looks:gender)

personality_gender <- update(looks_gender, .~. + personality:gender)

looks_personality <- update(personality_gender, .~. + looks:personality)

speed_date_model <- update(looks_personality, .~. + looks:personality:gender)

anova(baseline, looks_model, personality_model, gender_model, looks_gender, 
      personality_gender, looks_personality, speedDateModel)

summary(speed_date_model)

t <- summary(speed_date_model)$tTable[ ,4]
df <- summary(speed_date_model)$tTable[ ,3]

rcontrast(-1.20802, 108)
rcontrast(3.85315, 108)
rcontrast(-7.53968, 108)
rcontrast(-0.97891, 108)



##################################################
## ROBUST ANALYSIS FOR MIXED DESIGNS            ##
##################################################

df <- read.delim("~/UNLV-MIS-761/data/ProfilePicture.dat", header = TRUE)
names(df) <- c("case", "relationship_status", "With Man", "Alone")
df$row <- c(1:17, 1:23)

df_melt <- melt(df, id = c("case", "row", "relationship_status"), 
                measured = c("couple", "alone"))
names(df_melt) <- c("case", "row", "relationship_status", 
                    "profile_picture", "friend_requests")

df <- cast(df_melt, row ~ relationship_status + profile_picture, 
           value = "friend_requests")
df$row <- NULL
df

## tsplit() general form:
## tsplit(levels of factor A, levels of factor B, tr = 0.2)
tsplit(2, 2, df)

## sppba() general form:s
## sppba(levels of factor A, levels of factor B, data, est = mom, nboot = 2000)
sppba(2, 2, df, est = mom, nboot = 2000)
sppbb(2, 2, df, est = mom, nboot = 2000)
sppbi(2, 2, df, est = mom, nboot = 2000)

## If you want to compares medians then execute:
# sppba(2, 2, df, est = median, nboot = 2000)


