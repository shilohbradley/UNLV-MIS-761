##################################################
##                                              ##
## Chapter 18                                   ##
## Categorical data                             ##
##                                              ##
##################################################

## Install packages -----
## No new packages needed

## Load packages -----
library(gmodels)
library(MASS)

## Function definitions -----

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/cats.dat", header = TRUE)

## Manipulate the data -----
food <- c(10, 28)
affection <- c(114, 48)
cats_table <- cbind(food, affection)

## Cross Table -----

## CrossTable() general form
## CrossTable(predictor, outcome, fisher = TRUE, chisq = TRUE,
##            expected = TRUE, sresid = TRUE, format = "SAS"/"SPSS")
## and for a contingency table
## CrossTable(contingencyTable, fisher = TRUE, chisq = TRUE,
##            expected = TRUE, sresid = TRUE, format = "SAS"/"SPSS")

CrossTable(catsData$Training, catsData$Dance, fisher = TRUE, chisq = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")
CrossTable(catsTable, fisher = TRUE, chisq = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS")

##################################################
## LOGLINEAR ANALYSIS                           ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/CatsandDogs.dat", header = TRUE)
df

df_cats <- subset(df, Animal == "Cat")
df_dogs <- subset(df, Animal == "Dog")

## Cross Table -----
CrossTable(df_cats$Training, df_cats$Dance, sresid = TRUE, prop.t = FALSE,
           prop.c = FALSE, prop.chisq = FALSE, format = "SPSS")
CrossTable(df_dogs$Training, df_dogs$Dance, sresid = TRUE, prop.t = FALSE,
           prop.c = FALSE, prop.chisq = FALSE, format = "SPSS")

## Loglinear -----
## loglm() general form
## newmModel <- loglm(~ predictors, data = contingencyTable, fit = TRUE)

## xtabs() general form
## newTable <- xtabs(~ classifying variables, data = dataFrame)

cat_table <- xtabs(~ Training + Dance, data = df_cats)

cat_saturated <- loglm(~ Training + Dance + Training:Dance, 
                       data = cat_table, fit = TRUE)

cat_no_interaction <- loglm(~ Training + Dance, data = cat_table, fit = TRUE)

## Mosaic plot -----
## mosaicplot() general form
## mosaicplot(contingencyTable, shade = TRUE, main = "Title")

mosaicplot(cat_saturated$fitted, shade = TRUE, main = "Cats: Saturated Model")
mosaicplot(cat_no_interaction$fitted, shade = TRUE, main = "Cats: Expected Values")

## Loglinear analysis -----
cat_dog_contingency_table <- xtabs(~ Animal + Training + Dance, data = df)
caturated <- loglm(~ Animal * Training * Dance, data = cat_dog_contingency_table)
summary(caturated)

three_way <- loglm(~ Animal + Training + Dance + Animal:Training + Animal:Dance +
                     Dance:Training, data = cat_dog_contingency_table)
three_way <- update(caturated, .~. -Animal:Training:Dance)
summary(three_way)

anova(caturated, three_way)

training_dance <- update(three_way, .~. -Training:Dance)
animal_dance <- update(three_way, .~. -Animal:Dance)
animal_training <- update(three_way, .~. -Animal:Training)

## ANOVA -----
anova(three_way, training_dance)
anova(three_way, animal_dance)
anova(three_way, animal_training)

## Mosaic plot -----
mosaicplot(cat_dog_contingency_table, shade = TRUE, main = "Cats and Dogs")
