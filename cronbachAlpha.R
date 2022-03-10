#Cronbach alpha's for PMS study Mitchel Kappen
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



##### Clean data up a bit #####
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'

# Factorize and rename columns
data$PMS <- ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels
names(data)[names(data) == "allRRS"] = "RRS" # Rename column
data$Order <- factor(data$Order)

# Exclude everyone on the pill/copper spiral/other: only those with Natural Contraception are left included
data_allcontraception <- data # Backup the data prior to exclusion
data<-data[!(data$Contraception=="Pill"|data$Contraception=="other"|data$Contraception=="Hor. Coil"|data$Contraception=="Hor.Coil"),] # Only looking at non-hormonal contraceptives, so kick out all other data

###### Cronbach's Alpha #########

randn <-floor(runif(3, min=0, max=101)) #random numbers for sanity check

## DASS
dataC <- data.frame(select(data, matches("DASS21")))
for (i in randn){ #check if they are correctly calculated
  s1 <- data$DASS.Total[i]
  s2 <-as.integer(rowSums(dataC)[i])
  if (s1 != s2){print('Error!')}
}
cronbach.alpha(dataC, CI=TRUE)

#RRS
dataC <- data.frame(select(data, matches("RRS.R")))
for (i in randn){ #check if they are correctly calculated
  s1 <- data$RRS[i]
  s2 <-as.integer(rowSums(dataC)[i])
  if (s1 != s2){print('Error!')}
}
cronbach.alpha(dataC, CI=TRUE)

#BSRI
dataC <- data.frame(select(data, matches("BSRI")))
dataC <- dataC[,!(names(dataC)%in% c("folliculairBSRI", 'luteaalBSRI'))]
for (i in randn){ #check if they are correctly calculated
  s1<- data$folliculairBSRI[i]
  s2<- data.frame(select(data, matches("BSRI_folliculair")))
  s2 <-as.integer(rowSums(s2)[i])
  if (s1 != s2){print('Error!')}
}
cronbach.alpha(dataC, CI=TRUE, na.rm=T)

#PTQ
dataC <- data.frame(select(data, matches("PTQ")))
dataC <- dataC[,!(names(dataC)%in% c("folliculairPTQ", 'luteaalPTQ'))]
for (i in randn){ #check if they are correctly calculated
  s1<- data$folliculairPTQ[i]
  s2<- data.frame(select(data, matches("PTQ_folliculair")))
  s2 <-as.integer(rowSums(s2)[i])
  if (s1 != s2){print('Error!')}
}
cronbach.alpha(dataC, CI=TRUE, na.rm=T)

#PSS
dataC <- data.frame(select(data, matches("PSS")))
dataC <- dataC[,!(names(dataC)%in% c("folliculairPSS", 'luteaalPSS'))]

for (i in randn){ #check if they are correctly calculated
  s1<- data$folliculairPSS[i]
  s2<- data.frame(select(data, matches("PSS_folliculair")))
  s2 <-as.integer(rowSums(s2)[i])
  if (s1 != s2){print('Error!')}
}

cronbach.alpha(dataC, CI=TRUE, na.rm=T)

#PSST
dataC <- data.frame(select(data, matches("PST")))
cronbach.alpha(dataC, CI=TRUE)