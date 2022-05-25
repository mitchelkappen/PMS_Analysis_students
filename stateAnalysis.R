##############################
#                            #
# Analysis of State variables#
#         PMS data           #
#                            #
#############################
# This code uses premade csv for STATE specific variables and perform analysis and data viz
# Author: Mitchel Kappen & Sofie Raeymakers
# 10-3-2022
##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

# Install packages
list.of.packages <- c("lme4",'emmeans','tidyverse', 'car', 'ggplot2', 'lsr', 'ggpubr', 'effectsize') # All relevant packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # Check if any of these packages are not yet installed
if(length(new.packages)) install.packages(new.packages) # If any packages are not yet installed, install them
library(lme4) #linear models
library(emmeans) # estimated marginal means
library(tidyverse) # transform data
library(car) # anova
library(ggplot2) # figures
library(ggpubr) #correlations
library(effectsize)#phi
library(psych) # Cohen.d

##### General settings #####
# Get and declare functions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location - Else it can't find functions.R
source("functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together
options(contrasts = c("contr.sum","contr.poly")) # Important for coding of categorical variables in interactions

# Set WD
Dir = "Data/" #data from VPN folder

# Get data
data <- read.csv(paste0(Dir, "cleanedDataMoments.csv"), header = TRUE, sep = ) #upload data

# save figures
if (!dir.exists("Figures")){ # Create folder for storing the figures if it doesn't exist yet
  dir.create("Figures")}
plotPrefix <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Figures/") # Prefix to easily store figures later

##### Data Cleaning #####
# Use names for PMS score - easier interpretation and plotting later
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'
data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels

# Factorize the rest of the data where needed
data$ID <- factor(data$ID)
data$newid = factor(seq(unique(data$ID))) # This creates a new ID variable that takes a logical order from 1-length(ID)
data$Moment <- factor(data$Moment)
data$Contraception <- factor(data$Contraception)

# Exclude everyone on the pill/hormonal coil/other: only those with Natural Contraception + copper coil are left included
data_allcontraception <- data # Backup the data prior to exclusion
data <- data[!(data$Contraception=="Pill"| data$Contraception=="other"|data$Contraception=="Hor. Coil"|data$Contraception=="Hor.Coil"),] # Only looking at non-hormonal contraceptives, so kick out all other data

###### Analysis ######
#### State: PSS ####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'PSS ~ PMS * Moment + Age + (1|newid)' # No added effect for contraception, yes for age. # anova(d0.1, d0.2, test="Chisq")
d0.1 <- lmer(formula,data=dataModel)
modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

## Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

### Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d Moment = Follicular
print("Cohen's D for PSS at Follicular | PMS vs noPMS:")
cohens_d_state(data, 'PSS', 'Foll', 'PMS', 'noPMS') #PMS-noPMS

print("Cohen's D for PSS at Follicular | PMDD vs noPMS:")
cohens_d_state(data, 'PSS', 'Foll', 'PMDD', 'noPMS')

print("Cohen's D for PSS at Follicular | PMS vs PMDD:") 
cohens_d_state(data, 'PSS', 'Foll', 'PMS', 'PMDD')

# Cohen's d Moment = Luteal
print("Cohen's D for PSS at Luteal | PMS vs noPMS:") 
cohens_d_state(data, 'PSS', 'Lut', 'PMS', 'noPMS')

print("Cohen's D for PSS at Luteal | PMDD vs noPMS:") 
cohens_d_state(data, 'PSS', 'Lut', 'PMDD', 'noPMS')

print("Cohen's D for PSS at Luteal | PMS vs PMDD:") 
cohens_d_state(data, 'PSS', 'Lut', 'PMS', 'PMDD')

### Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts
# Cohen's d for FOllicular vs Luteal
print("Cohen's D for PSS Follicular vs Luteal | noPMS:") 
cohens_d_moments(data, 'PSS', 'noPMS')

print("Cohen's D for PSS Follicular vs Luteal | PMS:") 
cohens_d_moments(data, 'PSS', 'PMS')

print("Cohen's D for PSS Follicular vs Luteal | PMDD:") 
cohens_d_moments(data, 'PSS', 'PMDD')

## Visualisation
max_y<-max(data$PSS)
plot <- stateplot(data, emm0.2,'PSS', 'PSS') +
  # Follicular
  geom_segment(aes(x =0.9, y = max_y, xend = 1, yend = max_y), size= 1)+ # line bottom first
  annotate('text', x=0.95, y=max_y + max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =0.9, y = max_y+max_y/15, xend = 1.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=1, y=max_y+max_y/15+max_y/100, label='***', size=7)+ # star
  # Luteal  
  geom_segment(aes(x =1.9, y = max_y, xend = 2, yend = max_y), size= 1)+ #bottom first line
  annotate('text', x=1.95, y=max_y + max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =2, y = max_y+max_y/50, xend = 2.1, yend = max_y+max_y/50), size= 1)+ # bottom second line
  annotate('text', x=2.05, y=max_y+max_y/50+max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+# top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7) # star
ggsave(plot, file=paste0(plotPrefix, "PSS.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot
plot

#### State: PTQ ####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'PTQ ~ PMS * Moment + (1|newid)' # No effect for age, no effect for contraception
d0.1 <- lmer(formula,data=dataModel)
modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

## Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

## Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment , adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d Moment = Follicular
print("Cohen's D for PTQ at Follicular | PMS vs noPMS:") 
cohens_d_state(data, 'PTQ', 'Foll', 'PMS', 'noPMS') #PMS-noPMS

print("Cohen's D for PTQ at Follicular | PMS vs PMDD:") 
cohens_d_state(data, 'PTQ', 'Foll', 'PMS', 'PMDD')

print("Cohen's D for PTQ at Follicular | PMDD vs noPMS:") 
cohens_d_state(data, 'PTQ', 'Foll', 'PMDD', 'noPMS')

# Cohen's d Moment = Luteal
print("Cohen's D for PTQ at Luteal | PMS vs noPMS:") 
cohens_d_state(data, 'PTQ', 'Lut', 'PMS', 'noPMS')

print("Cohen's D for PTQ at Luteal | PMS vs PMDD:") 
cohens_d_state(data, 'PTQ', 'Lut', 'PMS', 'PMDD')

print("Cohen's D for PTQ at Luteal | PMDD vs noPMS:") 
cohens_d_state(data, 'PTQ', 'Lut', 'PMDD', 'noPMS')

## Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts
# Cohen's d for FOllicular vs Luteal
print("Cohen's D for PSS Follicular vs Luteal | noPMS:") 
cohens_d_moments(data, 'PTQ', 'noPMS')

print("Cohen's D for PSS Follicular vs Luteal | PMS:") 
cohens_d_moments(data, 'PTQ', 'PMS')

print("Cohen's D for PSS Follicular vs Luteal | PMDD:") 
cohens_d_moments(data, 'PTQ', 'PMDD')

## Visualisation
max_y<-max(data$PTQ)
plot <- stateplot(data, emm0.2, "PTQ", 'PTQ') +
  # Follicular
  geom_segment(aes(x =1, y = max_y+max_y/50, xend = 1.1, yend = max_y+max_y/50), size= 1)+ # bottom second line
  annotate('text', x=1.05, y=max_y + max_y/100+max_y/50, label='**', size=7)+
  geom_segment(aes(x =0.9, y = max_y+max_y/15, xend = 1.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=1, y=max_y+max_y/15+max_y/100, label='***', size=7)+
  # Luteal
  geom_segment(aes(x =1.9, y = max_y, xend = 2, yend = max_y), size= 1)+ # bottom first line 
  annotate('text', x=1.95, y=max_y+max_y/100, label='*', size=7)+
  geom_segment(aes(x =2, y = max_y+max_y/50, xend = 2.1, yend = max_y+max_y/50), size= 1)+ # bottom second line 
  annotate('text', x=2.05, y=max_y + max_y/100+max_y/50, label='***', size=7)+
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "PTQ.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot

###### Supplemental Analysis ######
supDir = 'Supplemental/Figures/'
##### State: BSRI #####
dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

formula <- 'BSRI ~ PMS * Moment + (1|newid)' # No added value to the model for Age nor contraception # anova(d0.1, d0.2, test="Chisq")
d0.1 <- lmer(formula,data=dataModel)
modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

## Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

## Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
# Cohen's d Moment = Follicular
print("Cohen's D for BSRI at Follicular | PMS vs noPMS:") 
cohens_d_state(data, 'BSRI', 'Foll', 'PMS', 'noPMS') #PMS-noPMS

print("Cohen's D for BSRI at Follicular | PMDD vs noPMS:") 
cohens_d_state(data, 'BSRI', 'Foll', 'PMDD', 'noPMS')

print("Cohen's D for BSRI at Follicular | PMS vs PMDD:")
cohens_d_state(data, 'BSRI', 'Foll', 'PMS', 'PMDD')

# Cohen's d Moment = Luteal
print("Cohen's D for BSRI at Luteal | PMS vs noPMS:") 
cohens_d_state(data, 'BSRI', 'Lut', 'PMS', 'noPMS')

print("Cohen's D for BSRI at Luteal | PMDD vs noPMS:") 
cohens_d_state(data, 'BSRI', 'Lut', 'PMDD', 'noPMS')

print("Cohen's D for BSRI at Luteal | PMS vs PMDD:") 
cohens_d_state(data, 'BSRI', 'Lut', 'PMS', 'PMDD')

## Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts
# Cohen's d for FOllicular vs Luteal
print("Cohen's D for BSRI Follicular vs Luteal | noPMS:") 
cohens_d_moments(data, 'BSRI', 'noPMS')

print("Cohen's D for BSRI Follicular vs Luteal | PMS:") 
cohens_d_moments(data, 'BSRI', 'PMS')

print("Cohen's D for BSRI Follicular vs Luteal | PMDD:") 
cohens_d_moments(data, 'BSRI', 'PMDD')

## Visualisation
max_y<-max(data$BSRI)
plot <- stateplot(data, emm0.2, 'BSRI', 'BSRI') +
  # Follicular
  geom_segment(aes(x =0.9, y = max_y, xend = 1, yend = max_y), size= 1)+ # line bottom first
  annotate('text', x=0.95, y=max_y + max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =0.9, y = max_y+max_y/15, xend = 1.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=1, y=max_y+max_y/15+max_y/100, label='**', size=7)+ # tar
  # Luteal
  geom_segment(aes(x =1.9, y = max_y, xend = 2, yend = max_y), size= 1)+ #bottom first line
  annotate('text', x=1.95, y=max_y + max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7) # star
ggsave(plot, file=paste0(supDir,"BSRI.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot
plot

##### Correlations #####
## PSS vs. PTQ
# noPMS 
PSS <- data$PSS[data$PMS=='noPMS']
PTQ <- data$PTQ[data$PMS=='noPMS']
x_lab= 'noPMS_PSS'
y_lab='noPMS_PTQ'
corr_PSS_PTQ_noPMS = overall_corr(PSS, PTQ, x_lab, y_lab)
ggsave(corr_PSS_PTQ_noPMS, file=paste0(supDir,"corr_PSS_PTQ_noPMS.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot
# PMS
PSS <- data$PSS[data$PMS=='PMS']
PTQ <- data$PTQ[data$PMS=='PMS']
x_lab= 'PMS_PSS'
y_lab='PMS_PTQ'
corr_PSS_PTQ_PMS = overall_corr(PSS, PTQ, x_lab, y_lab)
ggsave(corr_PSS_PTQ_PMS, file=paste0(supDir,"corr_PSS_PTQ_PMS.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot
# PMDD
PSS <- data$PSS[data$PMS=='PMDD']
PTQ <- data$PTQ[data$PMS=='PMDD']
x_lab= 'PMDD_PSS'
y_lab='PMDD_PTQ'
corr_PSS_PTQ_PMDD = overall_corr(PSS, PTQ, x_lab, y_lab)
ggsave(corr_PSS_PTQ_PMDD, file=paste0(supDir,"corr_PSS_PTQ_PMDD.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot

## Delta
# noPMS
PSS <- data$PSS[data$PMS=='noPMS'&data$Moment=='Foll']-data$PSS[data$PMS=='noPMS'&data$Moment=='Lut']
PTQ <- data$PTQ[data$PMS=='noPMS'&data$Moment=='Foll']-data$PTQ[data$PMS=='noPMS'&data$Moment=='Lut']
x_lab = 'noPMS_delta_PSS'
y_lab = 'noPMS_delta_PTQ'
corr_delta_PSS_PTQ_noPMS = overall_corr(PSS, PTQ, x_lab, y_lab)
ggsave(corr_delta_PSS_PTQ_noPMS, file=paste0(supDir,"corr_delta_PSS_PTQ_noPMS.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot
# PMS
PSS <- data$PSS[data$PMS=='PMS'&data$Moment=='Foll']-data$PSS[data$PMS=='PMS'&data$Moment=='Lut']
PTQ <- data$PTQ[data$PMS=='PMS'&data$Moment=='Foll']-data$PTQ[data$PMS=='PMS'&data$Moment=='Lut']
x_lab = 'PMS_delta_PSS'
y_lab = 'PMS_delta_PTQ'
corr_delta_PSS_PTQ_PMS = overall_corr(PSS, PTQ, x_lab, y_lab)
ggsave(corr_delta_PSS_PTQ_PMS, file=paste0(supDir,"corr_delta_PSS_PTQ_PMS.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot
#PMDD
PSS <- data$PSS[data$PMS=='PMDD'&data$Moment=='Foll']-data$PSS[data$PMS=='PMDD'&data$Moment=='Lut']
PTQ <- data$PTQ[data$PMS=='PMDD'&data$Moment=='Foll']-data$PTQ[data$PMS=='PMDD'&data$Moment=='Lut']
x_lab = 'PMDD_delta_PSS'
y_lab = 'PMDD_delta_PTQ'
corr_delta_PSS_PTQ_PMDD = overall_corr(PSS, PTQ, x_lab, y_lab)
ggsave(corr_delta_PSS_PTQ_PMDD, file=paste0(supDir,"corr_delta_PSS_PTQ_PMDD.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px") # save plot

