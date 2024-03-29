##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(yarrr)
library(lubridate)
library(lme4)
library(emmeans)
library(pander)
library(tidyverse)
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
nAGQ = 1 # When writing code, set to 0, when getting final results, set to 1�
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

# new variable PMSSCORE NEW iedereen pms 0 ook 0 iedereen die 1 OF 2 heeft wordt 1,
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'

data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels
# data$PMS <- as.factor(data$PMS)

data$RRS <- data$allRRS

# Factorize the rest of the data where needed
data$ID <- factor(data$ID)

data$PMSScore <- factor(data$PMSScore)
data$PMS <- factor(data$PMS)

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


# First make a variable containing all the right dates
data$Date[data$Moment=='Foll'] <- data$TrueFollicular[data$Moment=='Foll']
data$Date[data$Moment=='Lut'] <- data$TrueLuteal[data$Moment=='Lut']
# Convert this one column
data$Date = strptime(as.character(data$Date), "%d-%m-%Y")

data$Date <- as.Date(data$Date) # make it a date | MK: Wasnt it already a date?

startdate <- min(data$Date) #first date in dataset
data$Days <- as.integer(difftime(data$Date, startdate, units= 'days')) # NumDays variable: difference between current date and first date


#################### CORONA ###################


#lockdowns: 16 oktober tem 10 februari 2020. Data start op 08/26
#neem data van deze twee periodes samen en vergelijk met rest

data$Lockdown = 'Lockdown'
data$Lockdown[data$Date<'2020-10-16'] = 'no_Lockdown'
data$Lockdown[data$Date>'2021-02-10'] = 'no_Lockdown'
data$Lockdown<-factor(data$Lockdown)

formula <- 'allDisturbance ~ Lockdown + (1|Age) + (1|FirstMenstrual)'

rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=data)

d0.2 <- glmer(formula,data=data, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 10000000)),nAGQ = nAGQ)

d0.3 <- glmer(formula,data=data, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1,d0.2,d0.3)

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Lockdown", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ Lockdown, adjust ="fdr", type = "response")
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts


emm0.1 <- data.frame('allDisturbance'= emm0.1$emmean, 'Lockdown'=emm0.1$Lockdown)
max_y<-max(data$allDisturbance)
ggplot(data, aes(x = Lockdown, y =allDisturbance )) +
  geom_flat_violin(aes(fill=Lockdown),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
  geom_boxplot(aes(x = Lockdown, y = allDisturbance, fill = Lockdown), outlier.shape=NA, alpha= .45, width = .1, colour = "black")+
  geom_point(data= emm0.1, aes(x = Lockdown, y = allDisturbance, fill=Lockdown), size=4)+
  scale_colour_manual(values = c("blue", "red", "purple"))+
  ggtitle('allDisturbance~Lockdown')+
  # geom_segment(aes(x = 1, y=max_y, xend= 2, yend=max_y), size= 1)+
  # annotate('text', x=1.5, y=max_y+0.3, label='***', size=7)+
  # geom_segment(aes(x = 2, y=max_y+1, xend= 3, yend=max_y+1), size= 1)+
  # annotate('text', x=2.5, y=max_y+ 1.3, label='**', size=7)+
  # geom_segment(aes(x = 1, y=max_y+2, xend= 3, yend=max_y+2), size= 1)+
  # annotate('text', x=2, y=max_y+2.3, label='***', size=7)+
  scale_fill_manual(values = c("blue", 'red', 'purple'),
                    name='',labels=c('Lockdown \n n=166 ', 'no Lockdown \n n=214'))+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(
    legend.key.size=unit(1.3, 'cm'),
    legend.text=element_text(size=13),
    plot.title = element_text(size=rel(2)),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid.major.y = element_line( size=.1, color="#dedede" ),
    axis.text.x=element_text(size=rel(1.5)),
    axis.title.y=element_text(size=rel(1.4)),
    axis.title.x = element_blank())  