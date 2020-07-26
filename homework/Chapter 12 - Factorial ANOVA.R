##################################################
##                                              ##
## Chapter 12                                   ##
## Factorial ANOVA (GLM 3)                      ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("car","compute.es","ggplot2","multcomp","pastecs","reshape","WRS", repos="http://R-Forge.R-project.org"))

## Load packages -----
library(car)
library(compute.es)
library(ggplot2)
library(here)
library(multcomp)
library(pastecs)
library(reshape)
library(WRS)

## Function definitions -----
omega_factorial <- function(n, a, b, SSa, SSb, SSab, SSr) {
  
    MSa <- SSa / (a - 1)
    MSb <- SSb / (b - 1)
    MSab <- SSab / ((a - 1) * (b - 1))
    MSr <- SSr / (a * b * (n - 1))
    varA <- ((a - 1) * (MSa - MSr)) / (n * a * b)
    varB <- ((b - 1) * (MSb - MSr)) / (n * a * b)
    varAB <- ((a - 1) * (b - 1) * (MSab - MSr)) / (n * a * b)
    varTotal <- varA + varB + varAB + MSr
    
    print(paste("Omega-Squared A: ", varA / varTotal))
    print(paste("Omega-Squared B: ", varB / varTotal))
    print(paste("Omega-Squared AB: ", varAB / varTotal))
}

## Load data -----
df <- read.csv("~/UNLV-MIS-761/data/goggles.csv", header = TRUE)

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
df_model <- aov(attractiveness ~ gender + alcohol + gender:alcohol, data = df)
df_model <- aov(attractiveness ~ alcohol * gender, data = df)
Anova(df_model, type = "III")


## Post hoc analysis -----
pairwise.t.test(df$attractiveness, df$alcohol, p.adjust.method = "bonferroni")
postHocs <- glht(df_model, linfct = mcp(alcohol = "Tukey"))
summary(postHocs)
confint(postHocs)

## Plots in factorial ANOVA -----
plot(df_model)

## Robust factorial ANOVA -----
df$row <- rep(1:8, 6)

df_melt <- melt(df, id = c("row", "gender", "alcohol"), measured = "attractiveness") 

df_wide <- cast(df_melt, row ~ gender + alcohol)
df_wide$row <- NULL
df_wide

## t2way() general form:
## t2way(levels of factor A, levels of factor B, data, tr = 0.2, alpha = 0.05)
t2way(2, 3, df_wide)

## pbad2way() general form:
## pbad2way(levels of factor A, levels of factor B, data, est = mom, nboot = 2000)
pbad2way(2, 3, df_wide)

## If you want to compare median then execute
# pbad2way(2, 3, df_wide, est = median)

## Post hoc tests bases on a 20% trimmed mean
mcp2atm(2, 3, df_wide)

## Post hoc tests based on an M-estimator we execute
mcp2a(2, 3, df_wide)

## Calculating effect sizes -----
omega_factorial(8, 2, 3, 169, 3332, 1978, 3488)

## Compare men and women who drank no alcohol
mes(66.875, 60.625, 10.3293963, 4.95515604, 8, 8)
## Compare men and women who drank 2 pints
mes(66.875, 62.5, 12.5178444, 6.5465367, 8, 8)
## Compare men and women who drank 4 pints
mes(35.625, 57.5, 10.8356225, 7.0710678, 8, 8)

