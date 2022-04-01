#############################
#                           #
##  Combining code         ##
#                           #
#############################
# This code takes cleanedDataMoments.csv and combines it with the explicit emotional reactions stored in allPMSdata.csv
# Author: Mitchel Kappen
# 1-4-2022
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
library(lsr) # cohen's d
library(ggpubr) #correlations
library(effectsize)#phi

##### General settings #####

# Get and declare functions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location - Else it can't find functions.R
source("functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together

# Set WD
Dir = "Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\" #data from VPN folder
setwd(Dir)

# Get data
dataMoments <- read.csv(paste0(Dir, "06102021\\cleanedDataMoments.csv"), header = TRUE, sep = ) #upload data
dataEmotions <- read.csv(paste0(Dir, "06102021\\allPMSdata.csv"), header = TRUE, sep = ) #upload data

# Create Fol/Lut thingy
dataEmotions$MomentName = ''
dataEmotions$MomentName[dataEmotions$Order == 'A-B' & dataEmotions$Moment == 'A'] = 'Foll'
dataEmotions$MomentName[dataEmotions$Order == 'B-A' & dataEmotions$Moment == 'B'] = 'Foll'
dataEmotions$MomentName[dataEmotions$Order == 'A-B' & dataEmotions$Moment == 'B'] = 'Lut'
dataEmotions$MomentName[dataEmotions$Order == 'B-A' & dataEmotions$Moment == 'A'] = 'Lut'

# Create a column in dataEmotions with an identifier
dataEmotions$PSSNew = ''
dataEmotions$PTQNew = ''
dataEmotions$BSRINew = ''
# dataEmotions$PMSScore = ''

keyvalues = unique(dataMoments[ , c("participantNo", "Moment")])
for (i in 1:nrow(keyvalues)){
  pptNum = keyvalues$participantNo[i]
  moment = keyvalues$Moment[i]
  # print(pptNum)
  # print(moment)
  loc = which(dataEmotions$ID == pptNum & dataEmotions$MomentName == moment)
  locMoments = which(dataMoments$participantNo == pptNum & dataMoments$Moment == moment)
  # Now add new values to big dataframe
  dataEmotions$PSSNew[loc] = dataMoments$PSS[locMoments]
  dataEmotions$PTQNew[loc] = dataMoments$PTQ[locMoments]
  dataEmotions$BSRINew[loc] = dataMoments$BSRI[locMoments]
  # dataEmotions$PMSScore[loc] = dataMoments$PMSScore[locMoments]
  # break
}

# Check mismatch between python and R code. Aka; which data was not used in cleanedDataMoments.csv but was use in allPMSdata.csv
unusedData = dataEmotions[dataEmotions$BSRI != dataEmotions$BSRINew, ] # Only mismatches present because of the lack of a value from cleanedDataMoments. So double check for values is good
dataEmotions = dataEmotions[dataEmotions$BSRI == dataEmotions$BSRINew, ]

# Write data
write.csv(dataEmotions, paste0(Dir, "06102021/cleanEmotionReactionData.csv"), row.names = FALSE)
