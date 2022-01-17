##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(lme4)
library(emmeans)
library(tidyverse)
library(car)
library(ggplot2)

if (!dir.exists("figures"))
  dir.create("figures")
#####  General settings ##### 
nAGQ = 1 # When writing code, set to 0, when getting final results, set to 1ù
vpn = 1 # Set to 1 if using VPN

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location

# Get and declare functions
source("functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together

# Set WD
if (vpn == 1) {
  Dir = "Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\" #data from VPN folder
} else {
  Dir = "Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\" #data from github dir
}

setwd(Dir)
# Get data
data <-
  read.csv(paste0(Dir, "06102021\\cleanedDataTraits.csv"),
           header = TRUE,
           sep = ) #upload data

plotPrefix <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/figures paper/")

##### Clean data up a bit #####
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'

# Factorize and rename columns
data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels
names(data)[names(data) == "allRRS"] = "RRS" # Rename column
data$ID <- factor(data$ID)
data$Order <- factor(data$Order)

# Exclude everyone on the pill/copper spiral/other: only those with Natural Contraception are left included
data_allcontraception <- data # Backup the data prior to exclusion
data <- data[data$Contraception == "Natural", ] # Only looking at non-hormonal contraceptives, so kick out all other data

data$newid = factor(seq(unique(data$ID))) # This creates a new ID variable that takes a logical order from 1-length(ID)

##### ##### Statistics Time ##### ##### 
##### DASS ##### 
##### DASS: Depression #####
formula <- 'DASS_Depression ~ PMS + Age'

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lm(formula,data=data)
d0.2 <- glm(formula,data=data, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=data, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.3, type = 'III')

emmeans0.1 <- emmeans(d0.3, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

## Visualisation
max_y<-max(data$DASS_Depression)
plot <- traitplot(data, emm0.1, "DASS_Depression",'DASS:Depression') +
  geom_segment(aes(x = 1, y=max_y, xend= 2, yend=max_y), size= 1)+
    annotate('text', x=1.5, y=max_y+0.3, label='***', size=7)+
    geom_segment(aes(x = 2, y=max_y+1, xend= 3, yend=max_y+1), size= 1)+
    annotate('text', x=2.5, y=max_y+ 1.3, label='*', size=7)+
    geom_segment(aes(x = 1, y=max_y+2, xend= 3, yend=max_y+2), size= 1)+
    annotate('text', x=2, y=max_y+2.3, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "DASS_Depression_Plot.jpeg"), width = 2000, height = 1500, dpi = 300, units = "px")
plot

##### DASS: Anxiety ##### 
formula <- 'DASS_Anxiety ~ PMS + Age'

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lm(formula,data=data)
d0.2 <- glm(formula,data=data, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=data, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.2, type = 'III')

emmeans0.1 <- emmeans(d0.2, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

## Visualisation
max_y<-max(data$DASS_Anxiety)
plot <- traitplot(data, emm0.1, "DASS_Anxiety",'DASS:Anxiety') +
  geom_segment(aes(x = 1, y=max_y, xend= 2, yend=max_y), size= 1)+
  annotate('text', x=1.5, y=max_y+0.3, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y+1, xend= 3, yend=max_y+1), size= 1)+
  annotate('text', x=2.5, y=max_y+ 1.3, label='**', size=7)+
  geom_segment(aes(x = 1, y=max_y+2, xend= 3, yend=max_y+2), size= 1)+
  annotate('text', x=2, y=max_y+2.3, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "DASS_Anxiety.jpeg"), width = 2000, height = 1500, dpi = 300, units = "px")
plot

##### DASS: Stress ##### 
formula <- 'DASS_Stress ~ PMS + Age' # No effects found for Order - so removed as random intercept

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lm(formula,data=data)
d0.2 <- glm(formula,data=data, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=data, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.3, type = 'III')

emmeans0.1 <- emmeans(d0.3, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

## Visualisation
max_y<-max(data$DASS_Stress)
plot <- traitplot(data, emm0.1, "DASS_Stress",'DASS:Stress') +
  geom_segment(aes(x = 1, y=max_y, xend= 2, yend=max_y), size= 1)+
  annotate('text', x=1.5, y=max_y+0.3, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y+1, xend= 3, yend=max_y+1), size= 1)+
  annotate('text', x=2.5, y=max_y+ 1.3, label='**', size=7)+
  geom_segment(aes(x = 1, y=max_y+2, xend= 3, yend=max_y+2), size= 1)+
  annotate('text', x=2, y=max_y+2.3, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "DASS_Stress.jpeg"), width = 2000, height = 1500, dpi = 300, units = "px")
plot 

##### RRS ##### 
formula <- 'RRS ~ PMS + Age' # No effects found for Order - so removed as random intercept

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lm(formula,data=data)
d0.2 <- glm(formula,data=data, family = Gamma(link = "identity"))
d0.3 <- glm(formula,data=data, family = inverse.gaussian(link = "identity"))

modelNames = c('d0.1','d0.2','d0.3')

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC
print(paste0("Based on lowest AIC, the best fit was found in model: ", chosenModel))

Anova(d0.2, type = 'III')

emmeans0.1 <- emmeans(d0.2, pairwise ~ PMS, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

## Visualisation
max_y<-max(data$RRS)
plot <- traitplot(data, emm0.1, "RRS",'RRS') +
  geom_segment(aes(x = 1, y=max_y, xend= 2, yend=max_y), size= 1)+
  annotate('text', x=1.5, y=max_y+0.3, label='***', size=7)+
  geom_segment(aes(x = 2, y=max_y+1, xend= 3, yend=max_y+1), size= 1)+
  annotate('text', x=2.5, y=max_y+ 1.3, label='**', size=7)+
  geom_segment(aes(x = 1, y=max_y+2, xend= 3, yend=max_y+2), size= 1)+
  annotate('text', x=2, y=max_y+2.3, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "RRS.jpeg"), width = 2000, height = 1500, dpi = 300, units = "px")
plot