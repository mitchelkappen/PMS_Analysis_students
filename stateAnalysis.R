##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

#install packages
list.of.packages <- c("lme4",'emmeans','tidyverse', 'car', 'ggplot2', 'lsr', 'ggpubr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(lme4) #linear models
library(emmeans) # estimated marginal means
library(tidyverse) # transform data
library(car) # anova
library(ggplot2) # figures
library(lsr) # cohen's d
library(ggpubr) #correlations

#####  General settings #####
nAGQ = 1 # When writing code, set to 0, when getting final results, set to 1
vpn = 1 # Set to 1 if using VPN

# Get and declare functions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location - Else it can't find functions.R
source("functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together

# Set WD
if (vpn == 1) {
  Dir = "Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\" #data from VPN folder
} else {
  Dir = " " #data from github dir
}
setwd(Dir)

# Get data
data <-
  read.csv(paste0(Dir, "06102021\\cleanedDataMoments.csv"),
           header = TRUE,
           sep = ) #upload data

# save figures
if (!dir.exists("figures")) #create map for storing the figures
  dir.create("figures")
plotPrefix <- paste0(Dir, "figures/")
plotPrefix <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/figures paper/")

##### Data Cleaning #####
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'
data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels

# Factorize the rest of the data where needed
data$ID <- factor(data$ID)
data$newid = factor(seq(unique(data$ID))) # This creates a new ID variable that takes a logical order from 1-length(ID)
data$Moment <- factor(data$Moment)
data$Contraception <- factor(data$Contraception)

# Exclude everyone on the pill/copper spiral/other: only those with Natural Contraception are left included
data_allcontraception <- data # Backup the data prior to exclusion
data<-data[!(data$Contraception=="Pill"|data$Contraception=="other"|data$Contraception=="Hor. Coil"|data$Contraception=="Hor.Coil"),] # Only looking at non-hormonal contraceptives, so kick out all other data





##### States #####

### State: PSS ###
formula <- 'PSS ~ PMS * Moment + Age + (1|newid)' # No added effect for contraception, yes for age. # anova(d0.1, d0.2, test="Chisq")

dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

# Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

# Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

# Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts

## Visualisation
max_y<-max(data$PSS)
plot <- stateplot(data, emm0.2,'PSS', 'PSS') +
  # Follicular
  geom_segment(aes(x =0.9, y = max_y, xend = 1, yend = max_y), size= 1)+ # line bottom first
  annotate('text', x=0.95, y=max_y + max_y/100, label='*', size=7)+ # star
  # geom_segment(aes(x =1, y = max_y+max_y/50, xend = 1.1, yend = max_y+max_y/50), size= 1)+ # line bottom second
  # annotate('text', x=1.05, y=max_y+max_y/50+max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =0.9, y = max_y+max_y/15, xend = 1.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=1, y=max_y+max_y/15+max_y/100, label='***', size=7)+
  # Luteal  
  geom_segment(aes(x =1.9, y = max_y, xend = 2, yend = max_y), size= 1)+ #bottom first line
  annotate('text', x=1.95, y=max_y + max_y/100, label='*', size=7)+
  geom_segment(aes(x =2, y = max_y+max_y/50, xend = 2.1, yend = max_y+max_y/50), size= 1)+ # bottom second line
  annotate('text', x=2.05, y=max_y+max_y/50+max_y/100, label='*', size=7)+
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+# top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7)+
  geom_segment(aes(x =2, y = max_y+max_y/50, xend = 2.1, yend = max_y+max_y/50), size= 1)+ # bottom second line
  annotate('text', x=2.05, y=max_y+max_y/50+max_y/100, label='*', size=7)+
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+# top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "PSS_Plot.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot

##### State: BSRI #####
formula <- 'BSRI ~ PMS * Moment + (1|newid)' # No added value to the model for Age nor contraception # anova(d0.1, d0.2, test="Chisq")

dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

# Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

# Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

# Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts

## Visualisation
max_y<-max(data$BSRI)
plot <- stateplot(data, emm0.2, 'BSRI', 'BSRI') +
  # Follicular
  geom_segment(aes(x =0.9, y = max_y, xend = 1, yend = max_y), size= 1)+ # line bottom first
  annotate('text', x=0.95, y=max_y + max_y/100, label='*', size=7)+ # star
  geom_segment(aes(x =0.9, y = max_y+max_y/15, xend = 1.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=1, y=max_y+max_y/15+max_y/100, label='**', size=7)+
  # Luteal
  geom_segment(aes(x =1.9, y = max_y, xend = 2, yend = max_y), size= 1)+ #bottom first line
  annotate('text', x=1.95, y=max_y + max_y/100, label='*', size=7)+
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "BSRI_Plot.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
plot

##### State: PTQ #####
formula <- 'PTQ ~ PMS * Moment + (1|newid)' # No effect for age, no effect for contraception

dataModel = data
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

modelNames = c(d0.1) # Only d0.1 is taken into consideration due to zeroes being present

# Model Selection
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

# Between groups at time points
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment , adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

# Between timepoints for groups
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")
emm0.2 <- summary(emmeans0.2)$emmeans
emmeans0.2$contrasts

## Visualisation
max_y<-max(data$PTQ)
plot <- stateplot(data, emm0.2, "PTQ", 'PTQ') +
  # Follicular
  geom_segment(aes(x =1, y = max_y+max_y/50, xend = 1.1, yend = max_y+max_y/50), size= 1)+
  annotate('text', x=1.05, y=max_y + max_y/100+max_y/50, label='**', size=7)+
  geom_segment(aes(x =0.9, y = max_y+max_y/15, xend = 1.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=1, y=max_y+max_y/15+max_y/100, label='***', size=7)+
  # Luteal
  geom_segment(aes(x =1.9, y = max_y, xend = 2, yend = max_y), size= 1)+
  annotate('text', x=1.95, y=max_y+max_y/100, label='*', size=7)+
  geom_segment(aes(x =2, y = max_y+max_y/50, xend = 2.1, yend = max_y+max_y/50), size= 1)+
  annotate('text', x=2.05, y=max_y + max_y/100+max_y/50, label='***', size=7)+
  geom_segment(aes(x =1.9, y = max_y+max_y/15, xend = 2.1, yend = max_y+max_y/15), size= 1)+ # top line
  annotate('text', x=2, y=max_y+max_y/15+max_y/100, label='***', size=7)
ggsave(plot, file=paste0(plotPrefix, "PTQ_Plot.jpeg"), width = 2500, height = 1500, dpi = 300, units = "px")
# plot


##### Correlations #####

## noPMS 
PSS <- data$PSS[data$PMS=='noPMS']
PTQ <- data$PTQ[data$PMS=='noPMS']
x_lab= 'noPMS_PSS'
y_lab='noPMS_PTQ'
overall_corr(PSS, PTQ, x_lab, y_lab)

## PMS
PSS <- data$PSS[data$PMS=='PMS']
PTQ <- data$PTQ[data$PMS=='PMS']
x_lab= 'PMS_PSS'
y_lab='PMS_PTQ'
overall_corr(PSS, PTQ, x_lab, y_lab)

## PMDD
PMDD_PSS <- data$PSS[data$PMS=='PMDD']
PMDD_PTQ <- data$PTQ[data$PMS=='PMDD']
x_lab= 'PMDD_PSS'
y_lab='PMDD_PTQ'
overall_corr(PSS, PTQ, x_lab, y_lab)




  
  ##### Cohen's d (effect size) #####
  
  # using full formula:
  PSS_noPMS_mu <-mean(data$PSS[data$PMS=='noPMS'], na.rm=TRUE)
  PSS_PMS_mu<- mean(data$PSS[data$PMS=='PMS'], na.rm=TRUE)
  PSS_PMS_SD<- sd(data$PSS[data$PMS=='PMS'], na.rm=TRUE)
  (PSS_noPMS_mu-PSS_PMS_mu)/PSS_PMS_SD # d for PMS-noPMS =  -0.3663
  
  ### PSS ###
  
  
  ### Moment=Fol
  noPMS_mu <-mean(data$PSS[data$PMS=='noPMS' & data$Moment=='Foll'], na.rm=TRUE)
  PMS_mu <-mean(data$PSS[data$PMS=='PMS'& data$Moment=='Foll'], na.rm=TRUE)
  
  PMS_f <- as.numeric(as.character(data$PSS[data$PMS=='PMS'& data$Moment=='Foll']))
  PMDD_f <- as.numeric(as.character(data$PSS[data$PMS=='PMDD'& data$Moment=='Foll']))
  
  cohensD(PMS_f, mu= noPMS_mu)# effect size between PMS and noPMS group = 0.3663
  cohensD(PMDD_f, mu= noPMS_mu) # PMDD - noPMS
  cohensD(PMDD_f, mu= PMS_mu) #PMDD-PMS
  
  ### Moment=Lut
  noPMS_mu <-mean(data$PSS[data$PMS=='noPMS' & data$Moment=='Lut'], na.rm=TRUE)
  PMS_mu <-mean(data$PSS[data$PMS=='PMS'& data$Moment=='Lut'], na.rm=TRUE)
  
  PMS_f <- as.numeric(as.character(data$PSS[data$PMS=='PMS'& data$Moment=='Lut']))
  PMDD_f <- as.numeric(as.character(data$PSS[data$PMS=='PMDD'& data$Moment=='Lut']))
  
  cohensD(PMS_f, mu= noPMS_mu)# effect size between PMS and noPMS group = 0.3663
  cohensD(PMDD_f, mu= noPMS_mu) # PMDD - noPMS
  cohensD(PMDD_f, mu= PMS_mu) #PMDD-PMS
  
  ### FOll-Lut
  
  #noPMS
  lut <- mean(data$PSS[data$PMS=='noPMS' & data$Moment=='Lut'])
  fol <- as.numeric(as.character(data$PSS[data$PMS=='noPMS'& data$Moment=='Foll']))
  cohensD(fol, mu=lut)
  #PMS
  lut <- mean(data$PSS[data$PMS=='PMS' & data$Moment=='Lut'])
  fol <- as.numeric(as.character(data$PSS[data$PMS=='PMS'& data$Moment=='Foll']))
  cohensD(fol, mu=lut)
  #PMDD
  lut <- mean(data$PSS[data$PMS=='PMDD' & data$Moment=='Lut'])
  fol <- as.numeric(as.character(data$PSS[data$PMS=='PMDD'& data$Moment=='Foll']))
  cohensD(fol, mu=lut)
  
  
  ### PTQ ###
  
  ### Moment=Fol
  noPMS_mu <-mean(data$PTQ[data$PMS=='noPMS' & data$Moment=='Foll'], na.rm=TRUE)
  PMS_mu <-mean(data$PTQ[data$PMS=='PMS'& data$Moment=='Foll'], na.rm=TRUE)
  
  PMS_f <- as.numeric(as.character(data$PTQ[data$PMS=='PMS'& data$Moment=='Foll']))
  PMDD_f <- as.numeric(as.character(data$PTQ[data$PMS=='PMDD'& data$Moment=='Foll']))
  
  cohensD(PMS_f, mu= noPMS_mu)# effect size between PMS and noPMS group = 0.3663
  cohensD(PMDD_f, mu= noPMS_mu) # PMDD - noPMS
  cohensD(PMDD_f, mu= PMS_mu) #PMDD-PMS
  
  ### Moment=Lut
  noPMS_mu <-mean(data$PTQ[data$PMS=='noPMS' & data$Moment=='Lut'], na.rm=TRUE)
  PMS_mu <-mean(data$PTQ[data$PMS=='PMS'& data$Moment=='Lut'], na.rm=TRUE)
  
  PMS_f <- as.numeric(as.character(data$PTQ[data$PMS=='PMS'& data$Moment=='Lut']))
  PMDD_f <- as.numeric(as.character(data$PTQ[data$PMS=='PMDD'& data$Moment=='Lut']))
  
  cohensD(PMS_f, mu= noPMS_mu)# effect size between PMS and noPMS group = 0.3663
  cohensD(PMDD_f, mu= noPMS_mu) # PMDD - noPMS
  cohensD(PMDD_f, mu= PMS_mu) #PMDD-PMS
  
  
  ### FOll-Lut
  #noPMS
  lut <- mean(data$PTQ[data$PMS=='noPMS' & data$Moment=='Lut'])
  fol <- as.numeric(as.character(data$PTQ[data$PMS=='noPMS'& data$Moment=='Foll']))
  cohensD(fol, mu=lut)
  #PMS
  lut <- mean(data$PTQ[data$PMS=='PMS' & data$Moment=='Lut'])
  fol <- as.numeric(as.character(data$PTQ[data$PMS=='PMS'& data$Moment=='Foll']))
  cohensD(fol, mu=lut)
  #PMDD
  lut <- mean(data$PTQ[data$PMS=='PMDD' & data$Moment=='Lut'])
  fol <- as.numeric(as.character(data$PTQ[data$PMS=='PMDD'& data$Moment=='Foll']))
  cohensD(fol, mu=lut)