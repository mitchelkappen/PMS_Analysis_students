#### Settings & Packages #### -----------------------------------------------------
rm(list = ls())
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test
library(lme4)
library(lmerTest)
library(effects)
library(dplyr)
library(car)
library(emmeans)
library(fitdistrplus)
library(dplyr)
library(car)
library(MuMIn)
library(ggplot2)
library(ggsignif)
library(gridExtra)
library(tidyverse)
library(ggeffects)
library(pander)

#### IMPORT DATA & INSPECTION #### -------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# Set working directory to current directory
# setwd("C:\Users\ASUSTeK\OneDrive\2021-2022\internship\projects")
data <- read.table("Data/allPMSdata.csv",sep="\t", header=TRUE)

# Check whether R recognizes the variable types correctly
str(data)
data$Subject <- factor(data$Subject)
data$Type <- factor(data$Type)
data$Valence <- factor(data$Valence)
data$Group <- factor(data$Group)
data$Certainty <- factor(data$Certainty)
data$Phase <- factor(data$Phase)

# Exclude data?

# Define the formula for the model & check which model fits the data best
Formula_Time <- FixationDuration ~ Group*Valence*Sex*AOI + (1|Subject)

d0.1 <- lmer(Formula_Time,data=data)
d0.2 <- glmer(Formula_Time,data=data, family = gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)
d0.3 <- glmer(Formula_Time,data=data, family = gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)

d0.4 <- glmer(Formula_Time,data=data, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)
d0.5 <- glmer(Formula_Time,data=data, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)
d0.6 <- glmer(Formula_Time,data=data, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)

d0.7 <- glmer(Formula_Time,data=data, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)
d0.8 <- glmer(Formula_Time,data=data, family = inverse.gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)
d0.9 <- glmer(Formula_Time,data=data, family = inverse.gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=0)


# Remove the models that produced errors in the line below, the one with the lowest value is the best fitting model
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3), AIC(d0.4), AIC(d0.5), AIC(d0.6), AIC(d0.7), AIC(d0.8), AIC(d0.9))
emphasize.strong.cells(which(tabel == min(tabel), arr.ind = TRUE))
pandoc.table(tabel)








