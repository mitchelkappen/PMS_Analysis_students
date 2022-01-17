#################################
##### Isala x Ghep Analysis #####
#################################
##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(lme4)
library(emmeans)
library(tidyverse)
library(car)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# Set working directory to current directory
dataDir = "Z:/shares/ghepmk_data/2020_Kappen_PMS//"
dateDir = "06102021//"

##### Loading data #####
ghepData <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"IsalaDataSubset.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
ghepData <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"IsalaDataScreen.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
isalaData <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"Isala/PMS_UGENT_Q_for_mitchel.20220111.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
isalaData <- isalaData[isalaData$index != "ISALA00061", ] # Kicking out participant 000061 for now, because double entry
# Giving these columns a little more accessible names || WATCH OUT, PRONE TO ERROR
names(isalaData) <- c("index", "Leeftijd", "Day_in_Cycle", "Duratie_menstruatie", "Luteal", "Ovulation", "Follicular", "Poverty", "Kinderen_no.", "Adopted", "Pluskinderen", "Nervous_scale", "Nothing_to_cheer_up", "calm", "downhearted_sad", "happy", "stressed", "Nervous_dichotomous", "ra_shannon", "Anaerococcus", "Bifidobacterium", "Gardnerella", "Clostridium", "Finegoldia", "Lactobacillus.crispatus.group", "Lactobacillus.iners.group", "Lactobacillus.gasseri.group", "Lactobacillus.jensenii.group", "Limosilactobacillus", "Prevotella", "Streptococcus")

substrRight <- function(x, n){ # A function that takes the last n characters of a string
  substr(x, nchar(x)-n+1, nchar(x))}

# Make identical Isala identifiers
ghepData$IsalaNew <- substrRight(ghepData$Isala,5)
isalaData$IsalaNew <- substrRight(isalaData$index,5)

data <- merge(ghepData, isalaData, by = c("IsalaNew"))
