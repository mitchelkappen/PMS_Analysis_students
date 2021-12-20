##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(yarrr)
library(lme4)
library(emmeans)
library(pander)

library(reshape)
library(pander)
library(dplyr)
library(arrow)
library(car)
library(ggplot2)
library(effects)
library(ggsignif)
library(gridExtra) #gridarrange

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
  read.csv(paste0(Dir, "06102021\\cleanedDataMoments.csv"),
           header = TRUE,
           sep = ) #upload data

##### Clean data up a bit #####
#we make a new variable that has value 1 for the first TestMoment and 2 for the second TestMoment
#These moments were counterbalanced. when the order was B-A and the moment is B, this meanheas it is the first test moment, and vice versa for A-B and moment A.

# This part is not working because we don't have A and B in Moment here, but already foll and lut
data$TestMoment[data$Order == "A-B" & data$Moment == "A"] = 1# TestMoment 1 == Follicular phase
data$TestMoment[data$Order == "B-A" & data$Moment == "A"] = 2# TestMoment 2 == Luteal phase
data$TestMoment[data$Order == "A-B" & data$Moment == "B"] = 2
data$TestMoment[data$Order == "B-A" & data$Moment == "B"] = 1

# new variable PMSSCORE NEW iedereen pms 0 ook 0 iedereen die 1 OF 2 heeft wordt 1,
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'

data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels
# data$PMS <- as.factor(data$PMS)

# Factorize the rest of the data where needed
data$ID <- factor(data$ID)

data$PMSScore <- factor(data$PMSScore)
data$PMS <- factor(data$PMS)
data$Moment <-
  factor(data$Moment) # This removes "A and B", A == 1, B == 2 now
data$TestMoment <- factor(data$TestMoment)

# Exclude everyone on the pill/copper spiral/other: only those with Natural Contraception are left included
data_allcontraception <- data # Backup the data prior to exclusion
data <-
  data[!(
    data$Contraception == "Pill" |
      data$Contraception == "other" |
      data$Contraception == "Cop.Coil" |
      data$Contraception == "Hor. Coil" |
      data$Contraception == "Hor.Coil"
  ), ] # Delete all these columns

data$newid = factor(seq(unique(data$ID))) # This creates a new ID variable that takes a logical order from 1-length(ID)



##### States ##### 
##### State: PSS ##### 
formula <- 'PSS ~ PMS * Moment + (1|Age) + (1|newid)' # FirstMenstrual had zero effect so was removed from the model | Order showed no effect and was removed from model

dataModel = data

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 10000000)),nAGQ = nAGQ)

d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

# Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III') # Jens always uses anova() in stead of Anova() for lmer, but when doing so it ignores 'type' parameter. Need to figure out..
print("There is no significant interaction effect in the main model. However, we established these hypothesis before, so compare contrasts nonetheless")
plot(effect("PMS", chosenModel[[1]]))
plot(effect("PMS:Moment", chosenModel[[1]]))

# Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
print("All groups significantly differ from each other at every time point except PMS-PMDD at follicular. This is interesting, indicating that the both are not different non-PMS phase, but PMDD group gets more stress symptoms (so linked to PMs symptoms?)")

# Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="none", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts
print("Stress specifically doesn't increase for any group during the premenstrual phase as compared to the non-premenstrual phase")


## p-adjust! correct for multiple comparisons
# first add all p-values from the emmeans to 'p'
PMS_frame <- as.data.frame(emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response")$contrasts)[7]
Moment_frame <- as.data.frame(emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")$contrasts)[7]
p <- rbind(PMS_frame,Moment_frame)
p<-unlist(p)
p.adjust(p, method= 'fdr', n=9)

# Add custom fdr, get p-values from 2 different contrasts so we're comparing 9 ipv 15 tests
pd <- position_dodge(0.02) # move them .05 to the left and right

## Visualisation
ggplot(emm0.2, aes(x=Moment, y=emmean, color=PMS)) +
  geom_point(size = 1) + 
  geom_line(aes(group = PMS),size = 1)+
  geom_errorbar(width=.125, aes(ymin=emmean-SE, ymax=emmean+SE), position=pd)+
  theme_bw(base_size = 8)+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  ggtitle("PSS Stress")+
  labs(y = "PSS Stress", x= "Groups")

##### State: BSRI ##### 
formula <- 'BSRI ~ PMS * Moment + (1|FirstMenstrual)  + (1|newid)' # Order had zero effect so was removed from the model | Age showed no effect and was removed from model

dataModel = data

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 10000000)),nAGQ = nAGQ)

d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

# Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
print("There is no significant interaction effect in the main model. However, we established these hypothesis before, so compare contrasts nonetheless")
plot(effect("PMS", chosenModel[[1]])) # No idea why we're getting "PMS is not a high-order term in the model"
plot(effect("PMS:Moment", chosenModel[[1]]))

# Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="none", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
print("PMS and PMDD do not differ from each other at any time point, they do both differ from noPMS ")

# Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="none", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts
print("State Rumination specifically doesn't increase for any group during the premenstrual phase as compared to the non-premenstrual phase")

## p-adjust! correct for multiple comparisons
# first add all p-values from the emmeans to 'p'
PMS_frame <- as.data.frame(emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response")$contrasts)[7]
Moment_frame <- as.data.frame(emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")$contrasts)[7]
p <- rbind(PMS_frame,Moment_frame)
p<-unlist(p)
p.adjust(p, method= 'fdr', n=9)

pd <- position_dodge(0.02) # move them .05 to the left and right

## Visualisation
ggplot(emm0.2, aes(x=Moment, y=emmean, color=PMS)) +
  geom_point(size = 1) + 
  geom_line(aes(group = PMS),size = 1)+
  geom_errorbar(width=.125, aes(ymin=emmean-SE, ymax=emmean+SE), position=pd)+
  theme_bw(base_size = 8)+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  ggtitle("PSS Stress")+
  labs(y = "PSS Stress", x= "Groups")
