##############################
#                            #
# Analysis of Trait variables#
#         PMS data           #
#                            #
#############################
# This code uses premade csv for TRAIT specific variables and perform analysis and data viz
# Author: Mitchel Kappen & Sofie Raeymakers
# 10-3-2022
##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

#install packages
list.of.packages <- c("lme4",'emmeans','tidyverse', 'car', 'ggplot2', 'lsr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(lme4) #linear models
library(emmeans) # estimated marginal means
library(tidyverse) # transform data
library(car) # anova
library(ggplot2) # figures
library(lsr) #for calculating cohen's d

##### General settings #####
# Get and declare functions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location - Else it can't find functions.R
source("../functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together

# Set WD
Dir = "../Data/" #data from VPN folder

# Get data
data <- read.csv(paste0(Dir, "cleanedDataTraits.csv"), header = TRUE, sep = ) #upload data

# save figures
if (!dir.exists("Figures/noCopperCoil")){ # Create folder for storing the figures if it doesn't exist yet
  dir.create("Figures/noCopperCoil")}
plotPrefix <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Figures/noCopperCoil/") # Prefix to easily store figures later

##### Data Cleaning #####
# Use names for PMS score - easier interpretation and plotting later
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'
data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels

# Factorize the rest of the data where needed
data$ID <- factor(data$ID)
data$newid = factor(seq(unique(data$ID))) # This creates a new ID variable that takes a logical order from 1-length(ID)
data$Contraception <- factor(data$Contraception)
names(data)[names(data) == "allRRS"] = "RRS" # Rename column
data$Contraception <- factor(data$Contraception)

# Exclude everyone on the pill/hormonal coil/other: only those with Natural Contraception + copper coil are left included
data_allcontraception <- data # Backup the data prior to exclusion
data <- data[!(data$Contraception=="Pill"|data$Contraception == 'Cop. Coil'| data$Contraception=="other"|data$Contraception=="Hor. Coil"|data$Contraception=="Hor.Coil"),] # Only looking at non-hormonal contraceptives, so kick out all other data

###### Analysis ######
##### Trait: RRS #####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'RRS ~ PMS + Age' 
d0.1 <- lm(formula,data=dataModel)
d0.2 <- glm(formula,data=dataModel, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=dataModel, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.2, type = 'III')

emmeans0.1 <- emmeans(d0.2, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d
sprintf("Cohen's D for RRS | PMS vs noPMS: %.2f", cohens_d_trait(data$RRS, "noPMS", "PMS"))
sprintf("Cohen's D for RRS | PMS vs PMDD: %.2f", cohens_d_trait(data$RRS, "PMS", "PMDD"))
sprintf("Cohen's D for RRS | PMDD vs noPMS: %.2f", cohens_d_trait(data$RRS, "PMDD", "noPMS"))

## Visualisation
max_y<-max(data$RRS)
plot <- traitplot(data, emm0.1, "RRS",'RRS') +
  geom_segment(aes(x = 1, y=max_y, xend= 2, yend=max_y), size= 1)+ # bottom first line
  annotate('text', x=1.5, y=max_y+ max_y/100, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y+max_y/50, xend= 3, yend=max_y+max_y/50), size= 1)+ # bottom second line
  annotate('text', x=2.5, y=max_y+max_y/50+max_y/100, label='***', size=7)+
  geom_segment(aes(x = 1, y=max_y+max_y/15, xend= 3, yend=max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "noCopperCoil_RRS.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot
##### Trait: DASS #####
##### Trait: DASS - Depression #####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'DASS_Depression ~ PMS'
d0.1 <- lm(formula,data=dataModel)
d0.2 <- glm(formula,data=dataModel, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=dataModel, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.3, type = 'III')

emmeans0.1 <- emmeans(d0.3, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d
sprintf("Cohen's D for DASS-Depression | PMS vs noPMS: %.2f", cohens_d_trait(data$DASS_Depression, "noPMS", "PMS"))
sprintf("Cohen's D for DASS-Depression | PMS vs PMDD: %.2f", cohens_d_trait(data$DASS_Depression, "PMS", "PMDD"))
sprintf("Cohen's D for DASS-Depression | PMDD vs noPMS: %.2f", cohens_d_trait(data$DASS_Depression, "PMDD", "noPMS"))

## Visualisation
max_y1<-max(data$DASS_Depression)
plot1 <- traitplot(data, emm0.1, "DASS_Depression",'DASS:Depression') +
  geom_segment(aes(x = 1, y=max_y1, xend= 2, yend=max_y1), size= 1)+ # bottom first line
  annotate('text', x=1.5, y=max_y1+0.3, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y1+1, xend= 3, yend=max_y1+1), size= 1)+ # bottom second line
  annotate('text', x=2.5, y=max_y1+ 1.3, label='*', size=7)+
  geom_segment(aes(x = 1, y=max_y1+2, xend= 3, yend=max_y1+2), size= 1)+ # top line
  annotate('text', x=2, y=max_y1+2.3, label='***', size=7)
ggsave(plot1, file=paste0(plotPrefix, "noCopperCoil_DASS_Depression.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot1

##### Trait: DASS - Anxiety #####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'DASS_Anxiety ~ PMS + Age'

d0.1 <- lm(formula,data=dataModel)
d0.2 <- glm(formula,data=dataModel, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=dataModel, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.2, type = 'III')

emmeans0.1 <- emmeans(d0.2, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d
sprintf("Cohen's D for DASS-Anxiety | PMS vs noPMS: %.2f", cohens_d_trait(data$DASS_Anxiety, "noPMS", "PMS"))
sprintf("Cohen's D for DASS-Anxiety | PMS vs PMDD: %.2f", cohens_d_trait(data$DASS_Anxiety, "PMS", "PMDD"))
sprintf("Cohen's D for DASS-Anxiety | PMDD vs noPMS: %.2f", cohens_d_trait(data$DASS_Anxiety, "PMDD", "noPMS"))

## Visualisation
max_y2<-max(data$DASS_Anxiety)
plot2 <- traitplot(data, emm0.1, "DASS_Anxiety",'DASS:Anxiety') +
  geom_segment(aes(x = 1, y=max_y2, xend= 2, yend=max_y2), size= 1)+ # bottom first line
  annotate('text', x=1.5, y=max_y2+0.3, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y2+1, xend= 3, yend=max_y2+1), size= 1)+ # bottom second line
  annotate('text', x=2.5, y=max_y2+ 1.3, label='*', size=7)+
  geom_segment(aes(x = 1, y=max_y2+2, xend= 3, yend=max_y2+2), size= 1)+ # top line
  annotate('text', x=2, y=max_y2+2.3, label='***', size=7)
ggsave(plot2, file=paste0(plotPrefix, "noCopperCoil_DASS_Anxiety.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot2

##### Trait: DASS - Stress #####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'DASS_Stress ~ PMS + Age' 

d0.1 <- lm(formula,data=dataModel)
d0.2 <- glm(formula,data=dataModel, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=dataModel, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.3, type = 'III')

emmeans0.1 <- emmeans(d0.3, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d
sprintf("Cohen's D for DASS-Stress | PMS vs noPMS: %.2f", cohens_d_trait(data$DASS_Stress, "noPMS", "PMS"))
sprintf("Cohen's D for DASS-Stress | PMS vs PMDD: %.2f", cohens_d_trait(data$DASS_Stress, "PMS", "PMDD"))
sprintf("Cohen's D for DASS-Stress | PMDD vs noPMS: %.2f", cohens_d_trait(data$DASS_Stress, "PMDD", "noPMS"))

## Visualisation
max_y3<-max(data$DASS_Stress)
plot3 <- traitplot(data, emm0.1, "DASS_Stress",'DASS:Stress') +
  geom_segment(aes(x = 1, y=max_y3, xend= 2, yend=max_y3), size= 1)+ # bottom first line
  annotate('text', x=1.5, y=max_y3+0.3, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y3+1, xend= 3, yend=max_y3+1), size= 1)+ # bottom second line
  annotate('text', x=2.5, y=max_y3+ 1.3, label='**', size=7)+
  geom_segment(aes(x = 1, y=max_y3+2, xend= 3, yend=max_y3+2), size= 1)+ # top line
  annotate('text', x=2, y=max_y3+2.3, label='***', size=7)
ggsave(plot3, file=paste0(plotPrefix, "noCopperCoil_DASS_Stress.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot3

##### Combined all DASS Visualizations ##### 
library(patchwork)
# Create layout for combining the DASS plots
layout <- " 
###AAAAAA#D#
BBBBBBCCCCCC
"

fullDASSplot <- plot1 + plot2 + plot3 + guide_area() + plot_layout(design = layout, guides = "collect") + plot_annotation(tag_levels = c("A")) # Create plot

ggsave(fullDASSplot, file=paste0(plotPrefix, "noCopperCoil_DASS_all.jpeg"), width = 5000, height = 3500, dpi = 300, units = "px") # Save plot
fullDASSplot # Render plot
