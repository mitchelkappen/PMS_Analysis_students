##############################
#                            #
#  Cronbach's Alpha Analysis #
#         PMS data           #
#                            #
#############################
# This code calculates the Cronbach's Alpha's for all questionnaires in the PMs study
# Author: Sofie Raeymakers   & Mitchel Kappen
# 10-3-2022
##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(ltm)
library(dplyr)

#####  General settings ##### 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location - Else it can't find functions.R

# Set WD
Dir = "../Data/" #data from VPN folder

# Get data
data <- read.csv(paste0(Dir, "cleanData_allItems.csv"), header = TRUE, sep = ) # This datafile contains all individual responses to all individual items of each questionnaire

##### Data Cleaning #####
# Exclude everyone on the pill/hormonal coil/other: only those with Natural Contraception + copper coil are left included
data_allcontraception <- data # Backup the data prior to exclusion
data <- data[!(data$Contraception=="Pill"|data$Contraception=="other"|data$Contraception=="Hor. Coil"|data$Contraception=="Hor.Coil"),] # Only looking at non-hormonal contraceptives, so kick out all other data

###### Cronbach's Alpha #########
### DASS ####
DASSdata <- data[ , grepl( "DASS21" , names( data ) ) ] # Create dataframe only containing relevant data
cronbach.alpha(DASSdata, CI=TRUE)

### RRS ####
RRSdata <- data[ , grepl( "RRS.R" , names( data ) ) ] # Create dataframe only containing relevant data
cronbach.alpha(RRSdata, CI=TRUE)

### BSRI ####
BSRIdata <- data[ , grepl( "BSRI" , names( data ) ) ] # Create dataframe only containing relevant data
BSRIdata <- BSRIdata[,!(names(BSRIdata)%in% c("folliculairBSRI", 'luteaalBSRI'))] # Exclude two irrelevant (compounded) scores
colnames <- c("BSRI1","BSRI2","BSRI3","BSRI4","BSRI5","BSRI6","BSRI7","BSRI8") # Create column name variable to be able to combine two different time moments into one long dataframe
BSRIdatalong <- rbind(setNames(BSRIdata[,1:8], colnames), setNames(BSRIdata[,9:16], colnames)) # Combine luteal and follicular responses by adding rows under each other with new column names
cronbach.alpha(BSRIdatalong, CI=TRUE, na.rm=T)

### PTQ ####
PTQdata <- data[ , grepl( "PTQ" , names( data ) ) ] # Create dataframe only containing relevant data
PTQdata <- PTQdata[,!(names(PTQdata)%in% c("folliculairPTQ", 'luteaalPTQ'))] # Exclude two irrelevant (compounded) scores
colnames <- c("PTQ1","PTQ2","PTQ3","PTQ4","PTQ5","PTQ6","PTQ7","PTQ8","PTQ9","PTQ10","PTQ11","PTQ12","PTQ13","PTQ14","PTQ15") # Create column name variable to be able to combine two different time moments into one long dataframe
PTQdatalong <- rbind(setNames(PTQdata[,1:15], colnames), setNames(PTQdata[,16:30], colnames)) # Combine luteal and follicular responses by adding rows under each other with new column names
cronbach.alpha(PTQdatalong, CI=TRUE, na.rm=T)

### PSS ####
PSSdata <- data[ , grepl( "PSS" , names( data ) ) ] # Create dataframe only containing relevant data
PSSdata <- PSSdata[,!(names(PSSdata)%in% c("folliculairPSS", 'luteaalPSS'))] # Exclude two irrelevant (compounded) scores
colnames <- c("PSS1","PSS2","PSS3","PSS4","PSS5","PSS6","PSS7","PSS8","PSS9","PSS10")
PSSdatalong <- rbind(setNames(PSSdata[,1:10], colnames), setNames(PSSdata[,11:20], colnames))
cronbach.alpha(PSSdatalong, CI=TRUE, na.rm=T)

### PSST ####
PSSTdata <- data[ , grepl( "PST" , names( data ) ) ] # Create dataframe only containing relevant data
cronbach.alpha(PSSTdata, CI=TRUE)
