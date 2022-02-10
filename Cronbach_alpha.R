
  
#Cronbach alpha's for PMS study MItchel Kappen
#code by Sofie Raeymakers

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window


library(ltm)
library(dplyr)

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
  read.csv(paste0(Dir, "06102021\\cleanData_allItems.csv"),
           header = TRUE,
           sep = ) #upload data


#contains all individual items. 
#sanity check: count up items to see if scores are the same

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
data<-data[!(data$Contraception=="Pill"|data$Contraception=="other"|data$Contraception=="Hor. Coil"|data$Contraception=="Hor.Coil"),] # Only looking at non-hormonal contraceptives, so kick out all other data

data$newid = factor(seq(unique(data$ID))) # This creates a new ID variable that takes a logical order from 1-length(ID)

#Cronbach's Alpha

head(data)

dataC <- data.frame(select(data, matches("DASS21")))
cronbach.alpha(dataC, CI=TRUE)

dataC <- data.frame(select(data, matches("RRS.R")))
cronbach.alpha(dataC, CI=TRUE)

dataC <- data.frame(select(data, matches("BSRI")))
dataC <- dataC[,!(names(dataC)%in% c("folliculairBSRI", 'luteaalBSRI'))]
cronbach.alpha(dataC, CI=TRUE)

dataC <- data.frame(select(data, matches("PTQ")))
dataC <- dataC[,!(names(dataC)%in% c("folliculairPTQ", 'luteaalPTQ'))]
cronbach.alpha(dataC, CI=TRUE)

dataC <- data.frame(select(data, matches("PSS")))
dataC <- dataC[,!(names(dataC)%in% c("folliculairPSS", 'luteaalPSS'))]
cronbach.alpha(dataC, CI=TRUE)



