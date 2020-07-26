##################################################
##                                              ##
## Chapter 17                                   ##
## Exploratory Factor Analysis                  ##
##                                              ##
##################################################

## Install packages -----
# install.packages(c("corpcor", "GPArotation", "psych"))

## Load packages -----
library(corpcor)
library(GPArotation)
library(psych)

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/raq.dat", header = TRUE)

## Manipulate the data -----
raq_matrix <- cor(df)
round(raq_matrix, 2)

## Bartlett's test -----
## Both of these produce the same result
cortest.bartlett(df)
cortest.bartlett(raq_matrix, n = 2571)

## KMO -----
KMO(df)

## Both of these produce the same results
det(raq_matrix)
det(cor(df))

## Factor extraction -----
## principal() general form
## pcModel <- principal(dataframe/R-matrix, nfactors = number of factors, 
##                      rotate = "method of rotation", scores = TRUE/FALSE)

## Use either one of these commands
## They produce the same results
pc1 <- principal(df, nfactors = 23, rotate = "none")
pc1
# pc1 <- principal(raq_matrix, nfactors = 23, rotate = "none")
# pc1

plot(pc1$values, type = "b")

pc2 <- principal(df, nfactors = 4, rotate = "none")
pc2
# pc2 <- principal(raq_matrix, nfactors = 4, rotate = "none")

factor.model(pc2$loadings)

residuals <- factor.residuals(raq_matrix, pc2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])

large.resid <- abs(residuals) > 0.05
sum(large.resid)
sum(large.resid) / nrow(residuals)
sqrt(mean(residuals ^ 2))
hist(residuals)

## Rotation -----
## Orthogonal rotation (varimax)
pc3 <- principal(df, nfactors = 4, rotate = "varimax")
pc3 <- principal(raq_matrix, nfactors = 4, rotate = "varimax")

print.psych(pc3, cut = 0.3, sort = TRUE)

## Oblique rotation
pc4 <- principal(df, nfactors = 4, rotate = "oblimin")
pc4 <- principal(raq_matrix, nfactors = 4, rotate = "oblimin")

print.psych(pc4, cut = 0.3, sort = TRUE)

pc4$loadings %*% pc4$Phi

## factor.structure() general form
## factor.structure(pcModel, cut = 0.2, decimals = 2)
factor.structure(pc4, cut = 0.3) ## This function doesn't exist???
 
## Factor scores -----
pc5 <- principal(df, nfactors = 4, rotate = "oblimin", scores = TRUE)
pc5$scores
head(pc5$scores, 10)

df <- cbind(df, pc5$scores)

## Reliability analysis -----
computer_fear <- df[, c(6, 7, 10, 13, 14, 15, 18)]
statistics_fear <- df[, c(1, 3, 4, 5, 12, 16, 20, 21)]
math_fear <- df[, c(8, 11, 17)]
peer_evaluation <- df[, c(2, 9, 19, 22, 23)]

alpha(computer_fear)
alpha(statistics_fear, keys = c(1, -1, 1, 1, 1, 1, 1, 1))
alpha(math_fear)
alpha(peer_evaluation)

