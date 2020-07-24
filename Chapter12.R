## Install packages -----
# install.packages(c("car","compute.es","ggplot2","multcomp","pastecs","reshape","WRS", repos="http://R-Forge.R-project.org"))

## Load packages -----
library(car)
library(compute.es)
library(ggplot2)
library(multcomp)
library(pastecs)
library(reshape)
library(WRS)

## Set working directory -----
setwd("C:/Users/Shilerz/Desktop/MIS 761/")

## Load data -----
df <- read.csv("goggles.csv", header = TRUE)

## Manipulate data -----
df$alcohol <- factor(df$alcohol, levels = c("None", "2 Pints", "4 Pints"))

## General form:
## factor <- gl(number of levels, cases in each level, total cases, labels = c("label1", "label2")...)
gender <- gl(2, 24, labels = c("Female", "Male"))

alcohol <- gl(3, 8, 48, labels = c("None", "2 Pints", "4 Pints"))

attractiveness <- c(65, 70, 60, 60, 60, 55, 60, 55, 70, 65, 60, 70, 65, 60, 
                    60, 50, 55, 65, 70, 55, 55, 60, 50, 50, 50, 55, 80, 65, 
                    70, 75, 75, 65, 45, 60, 85, 65, 70, 70, 80, 60, 30, 30, 
                    30, 55, 35, 20, 45, 40)

## Combine the data into one data frame
df <- data.frame(gender, alcohol, attractiveness)

## Exploring the data -----
by(df$attractiveness, df$gender, stat.desc)
by(df$attractiveness, df$alcohol, stat.desc)
by(df$attractiveness, list(df$alcohol, df$gender), stat.desc)

## Levene's test
leveneTest(df$attractiveness, df$gender, center = median)
leveneTest(df$attractiveness, df$alcohol, center = median)
leveneTest(df$attractiveness, interaction(df$alcohol, df$gender), center = median)

## Contrasts
contrasts(df$alcohol) <- cbind(c(-2, 1, 1), c(0, -1, 1))
contrasts(df$gender) <- c(-1, 1)

## Fitting a factorial ANOVA model -----
gogglesModel <- aov(attractiveness ~ gender + alcohol + gender:alcohol, data = df)
gogglesModel <- aov(attractiveness ~ alcohol * gender, data = df)
Anova(gogglesModel, type = "III")


## Post hoc analysis -----
pairwise.t.test(df$attractiveness, df$alcohol, p.adjust.method = "bonferroni")
postHocs <- glht(gogglesModel, linfct = mcp(alcohol = "Tukey"))
summary(postHocs)
confint(postHocs)
  