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

## Read in data -----
df <- read.delim(here("~/Documents/GitHub/UNLV-MIS-761/Bushtucker.dat"), header = TRUE)

## Manipulate data -----
longBush <- melt(df, id = "participant", measured = c("stick_insect", "kangaroo_testicle", "fish_eye", "witchetty_grub"))
names(longBush) <- c("Participant", "Animal", "Retch")
longBush$Animal <- factor(longBush$Animal, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))
longBush <- longBush[order(longBush$Participant),]