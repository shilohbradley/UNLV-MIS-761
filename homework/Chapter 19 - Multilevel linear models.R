##################################################
##                                              ##
## Chapter 19                                   ##
## Multilevel linear models                     ##
##                                              ##
##################################################

## Install packages -----
## No new packages needed

## Load packages -----
library(car)
library(ggplot2)
library(nlme)
library(reshape)

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Cosmetic Surgery.dat", header = TRUE)

## Linear model -----
df_lm <- lm(Post_QoL ~ Surgery, data = df)
summary(df_lm)

df_lm <- lm(Post_QoL ~ Surgery + Base_QoL, data = df)
summary(df_lm)

## Generalized least squares -----
intercept_only <- gls(Post_QoL ~ 1, data = df, method = "ML")
summary(intercept_only)

random_intercept_only <- lme(Post_QoL ~ 1, data = df, random = ~1|Clinic, 
                             method = "ML")
summary(random_intercept_only)

logLik(intercept_only)*-2
logLik(random_intercept_only)*-2

anova(intercept_only, random_intercept_only)

random_intercept_surgery <- lme(Post_QoL ~ Surgery, data = df,
                                random = ~1|Clinic, method = "ML")
summary(random_intercept_surgery)

random_intercept_surgery_QoL <- lme(Post_QoL ~ Surgery + Base_QoL,
                                    data = df, random = ~1|Clinic, method = "ML")
summary(random_intercept_surgery_QoL)

anova(random_intercept_only, random_intercept_surgery, random_intercept_surgery_QoL)

## Random slopes -----
random_slope <- lme(Post_QoL ~ Surgery + Base_QoL, data = df, 
                    random = ~Surgery|Clinic, method = "ML")
summary(add_random_slope)
anova(random_intercept_surgery_QoL, random_slope)

reason <- lme(Post_QoL ~ Surgery + Base_QoL + Reason, data = df,
              randome = ~Surgery|Clinic, method = "ML")

final_model <- lme(Post_QoL ~ Surgery + Base_QoL + Reason + Surgery:Reason,
                   data = df, random = ~Surgery|Clinic, method = "ML")

anova(random_slope, reason, final_model)

intervals(final_model, 0.90)
intervals(final_model, 0.95)
intervals(final_model, 0.99)

physical_subset <- df$Reason == 1
cosmetic_subset <- df$Reason == 0

physical_model <- lme(Post_QoL ~ Surgery + Base_QoL, data = df, 
                      random = ~Surgery|Clinic, subset = physical_subset, 
                      method = "ML")

cosmetic_model  <- lme(Post_QoL ~ Surgery + Base_QoL, data = df,
                       random = ~Surgery|Clinic, subset = cosmetic_subset,
                       method = "ML")

summary(physical_model)
summary(cosmetic_model)

##################################################
## GROWTH MODELS                                ##
##################################################

## Read in data -----
df <- read.delim("~/UNLV-MIS-761/data/Honeymoon Period.dat", header = TRUE)

## Manipulate the data -----
df_melt <- melt(df, id = c("Person", "Gender"))

## Build the model -----
intercept <- gls(value ~ 1, data = df_melt, method = "ML", na.action = na.exclude)

random_intercept <- lme(value ~ 1, data = df_melt, random = ~1|Person, 
                        method = "ML", na.action = na.exclude, 
                        control = list(opt = "optim"))

## Adding in time as a fixed effect -----
time_RI <- update(random_intercept, .~. + Time)

## Random slopes -----
time_RS <- update(time_RI, random = ~Time|Person)

## Modelling the covariance structure -----
AR_model <- update(time_RS, correlation = corAR1(0, form = ~Time|Person))

## Comparing models -----
anova(intercept, random_intercept, time_RI, time_RS, AR_model)

summary(AR_model)

intervals(AR_model)

## Adding higher-order polynomials -----
time_quadratic <- update(AR_model, .~. + I(Time ^ 2))
time_cubic <- update(time_quadratic, .~. + I(Time ^ 3))

anova(AR_model, time_quadratic, time_cubic)

summary(time_cubic)
intervals(time_cubic)

poly_model <- update(AR_model, .~ ploy(Time, 3))



