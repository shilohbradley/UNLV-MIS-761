##################################################
##                                              ##
## Chapter 15                                   ##
## Non-parametric tests                         ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("clinfun", "pgirmess"))

## Load packages -----
library(clinfun)
library(ggplot2)
library(pastecs)
library(pgirmess)

## Function definitions -----
rFromWilcox <- function(wilcoxModel, N) {
  
    z <- qnorm(wilcoxModel$p.value / 2)
    r <- z / sqrt(N)
    cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Drug.dat", header = TRUE)

##################################################
## WILCOXON TEST                                ##
##################################################

## wilcox.text() general form
## newModel <- wilcox.test(outcome ~ predictor, data = dataFrame, 
##                         paired = FALSE / TRUE)
## newModel <- wilcox.test(scores group 1, scores group 2, 
##                         paired = FALSE / TRUE)

## Basic tests -----
sun_model <- wilcox.test(sundayBDI ~ drug, data = df)
sun_model

wed_model <- wilcox.test(wedsBDI ~ drug, data = df)
wed_model

## To use a normal approximation rather than an exact p
## and to get rid of the continuity correction
sun_model <- wilcox.test(sundayBDI ~ drug, data = df, exact = FALSE, 
                         correct = FALSE)
sun_model

wed_model <- wilcox.test(wedsBDI ~ drug, data = df, exact = FALSE, 
                         correct = FALSE)
wed_model

## Calculate the effect size -----
rFromWilcox(sun_model, 20)
rFromWilcox(wed_model, 20)

##################################################
## WILCOXON SIGNED-RANK TEST                    ##
##################################################

## Subset the data to create separate data frames -----
df_alcohol <- subset(df, subset = drug == "Alcohol")
df_ecstasy <- subset(df, subset = drug == "Ecstasy")

## Build the models -----
alcohol_model <- wilcox.test(df_alcohol$wedsBDI, df_alcohol$sundayBDI, 
                             paired = TRUE, correct = FALSE)
alcohol_model

ecstasy_model <- wilcox.test(df_ecstasy$wedsBDI, df_ecstasy$sundayBDI, 
                             paired = TRUE, correct = FALSE)
ecstasy_model

## Calculate the effect size -----
rFromWilcox(alcohol_model, 20)
rFromWilcox(ecstasy_model, 20)

##################################################
## KRUSKAL-WALLIS TEST                          ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Soya.dat", header = TRUE)

## Manipulate the data -----
# df$Soya <- factor(df$Soya, levels = levels(df$Soya)[c(4, 1, 2, 3)])

## Build the model -----
## kruskal.test() general form
## newModel <- kruskal.test(outcome ~ predictor, data = dataFrame, 
##                          na.action = "an.action")

kruskal.test(Sperm ~ Soya, data = df)

df$Ranks <- rank(df$Sperm)

by(df$Ranks, df$Soya, mean)

## Build a plot -----
ggplot(df, aes(x = Soya, y = Sperm, group = Soya)) +
  geom_boxplot()

## Post hoc tests -----
kruskalmc(Sperm ~ Soya, data = df)
kruskalmc(Sperm ~ Soya, data = df, cont = "two-tailed")

##################################################
## JONCKHEERE-TERPSTRA TEST                     ##
##################################################

## jonckheere.test() general form
## jonckheere.test(outcome variable, group variable (as numbers))

jonckheere.test(df$Sperm, as.numeric(df$Soya))

##################################################
## FRIEDMAN'S ANOVA                             ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Diet.dat", header = TRUE)

## Manipulate the data -----
df_complete <- na.omit(df)

## Build the models -----
friedman.test(as.matrix(df))
diet_model <- wilcox.test(df$Start, df$Month1, paired = TRUE, correct = FALSE)
diet_model

## Post hoc tests -----
friedmanmc(as.matrix(df))

## Calculate the effect size -----
rFromWilcox(diet_model, 20)


