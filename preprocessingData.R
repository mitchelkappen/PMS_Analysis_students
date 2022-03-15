#############################
#                           #
##  PreProcessing PMS data ##
#                           #
#############################
# This code gets raw data and turns it into usable csv files - see ReadMe.md
# Author: Mitchel Kappen
# 10-3-2022
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

###### Declare workspace and load data #####
list.of.packages <- c("dplyr",'tidyr') # All relevant packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # Check if any of these packages are not yet installed
if(length(new.packages)) install.packages(new.packages) # If any packages are not yet installed, install them
library(dplyr)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location - Else it can't find functions.R
source("functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together

dataDir = "Z:/shares/ghepmk_data/2020_Kappen_PMS//"
dateDir = "06102021//"

DataFrame <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"results-survey987313.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE)) # Load csv from Screening moment
DataFrame <- select(DataFrame, -c(lastpage, startlanguage, startdate, datestamp, IFC1, IFC2, IFC3, IFC4, IFC5, IFC6, auto1.SQ001., AgeValidation.SQ001., AgeValidation.SQ002., interviewtime, groupTime17, IFC1Time, IFC2Time, IFC3Time, IFC4Time, IFC5Time, IFC6Time, auto1Time, groupTime13, GenderTime, AgeTime, MenstruationTime, FirstMenstrualTime, RegularMentrualTime, MenopauseTime, PregnantTime, PostPregnantTime, ContraceptiveTime, DutchTime, HormonesTime, MentalTime, LaptopTime, MenstrualToelichtTime, CurrentMensesTime, MenstrualStartTime, MenstrualEndTime, MenstrualEndExpectedTime, MenstrualDurationTime, AgeValidationTime, EMailTime, groupTime16, SymptomsTime, DisturbanceTime, SymptomsPRETime, groupTime14, RRSTime, groupTime15, DASS21Time )) #Remove columns with irrelevant data

DataFrame <- DataFrame[!(is.na(DataFrame$submitdate) | DataFrame$submitdate==""), ] # Clean up data --> remove all rows without submitdate, since that means they didnt complete the screening
DataFrame <- DataFrame[-c(which(DataFrame$ï..id == 2684)), ] # remove this one participant - not included because slipped through prescreening exclusion

DataFrameClean <- data.frame(select(DataFrame, c(ï..id, Age, FirstMenstrual, MenstrualStart, MenstrualEnd, MenstrualEndExpected, MenstrualDuration))) # Create dataframe with minimal information

######## Everything related to screening data ######
ExcelPMS <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"Participant-Excel.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
ExcelPMS <- ExcelPMS %>% mutate_all(~ replace_na(.x, "")) # Replace all NA's with empty lines - easier to work with

# Add a column with the actual testing moments so we can verify whether they did it on time
ExcelPMS$TrueFollicular[ExcelPMS$Test.gemist == "TRUE"] = ExcelPMS$Nieuwe.folliculaire.fase[ExcelPMS$Test.gemist == "TRUE"] 
ExcelPMS$TrueFollicular[ExcelPMS$Test.gemist == ""] = ExcelPMS$folliculaire.fase[ExcelPMS$Test.gemist == ""]

ExcelPMS$TrueLuteal[ExcelPMS$Test.gemist == "TRUE"] = ExcelPMS$Nieuwe.luteale.fase[ExcelPMS$Test.gemist == "TRUE"]
ExcelPMS$TrueLuteal[ExcelPMS$Test.gemist == ""] = ExcelPMS$luteale.fase[ExcelPMS$Test.gemist == ""] 

Randomisatie <- select(ExcelPMS, Entry.nummer, email, Participantnummer, Randomisatie, Exclusie, TrueFollicular, TrueLuteal, duur.cyclus) # Create dataframe with only crucial information for randomisation verification

# Trim the email addresses because some have whitespace at the end
DataFrame$EMail <- trimws(DataFrame$EMail)
Randomisatie$email <- trimws(Randomisatie$email)

DataFrame$testVolgorde = ''
DataFrame$participantID = ''
for (i in 1:nrow(DataFrame)){ # Loop over all participant rows that filled out screening completely
  loc = which(Randomisatie$email == DataFrame$EMail[i]) # Check for location of their entry number in the participant Excel file
  
  if (length(loc) == 0) {
    print(paste0("Something going on with participant ",toString(DataFrame$ï..id[i])," AKA entrynumber " ))
  } else {
    DataFrame$testVolgorde[i] = Randomisatie$Randomisatie[loc] # Use this location to grab their randomisation and participantNumber allocated
    DataFrame$participantID[i] = Randomisatie$Participantnummer[loc]
    DataFrame$Exclusie[i] = Randomisatie$Exclusie[loc]
    DataFrame$TrueFollicular[i] = Randomisatie$TrueFollicular[loc]
    DataFrame$TrueLuteal[i] = Randomisatie$TrueLuteal[loc]
    
    if (DataFrameClean$MenstrualDuration[i] != Randomisatie$duur.cyclus[loc]){ # If the durations of cycle don't match, this often means the participant didnt understand the question properly. So overwrite this with manual data entered after correspondence with participant
      DataFrameClean$MenstrualDuration[i] = Randomisatie$duur.cyclus[loc] # Add participants menstrual duration to clean dataframe
    }
  }
}

# Add variables to Clean Dataframe
Order <- DataFrame$testVolgorde
participantNo <- DataFrame$participantID
Exclusie <- DataFrame$Exclusie
TrueFollicular <- DataFrame$TrueFollicular
TrueLuteal <- DataFrame$TrueLuteal
DataFrameClean <- cbind(DataFrameClean, participantNo, Order, Exclusie, TrueFollicular, TrueLuteal)

###### Questionnaires - Trait #######
#### PSST ####
# PSST - Symptoms subscale
SymptomsData <- DataFrame[ , grepl( "Symptoms.PST" , names( DataFrame ) ) ] # Make dataset with only Symptoms variables
SymptomsData <- cbind(SymptomsData, Symptoms.PST04=c(DataFrame$Symptoms.SPST04))

SymptomsData <- SymptomsData[,c(1,2,3,14,4,5,6,7,8,9,10,11,12,13)] # Re-order columns to match the actual questionnaire

# Create dataframe with all individual responses
SymptomsDataNumeric <- SymptomsData
SymptomsDataNumeric[,1:ncol(SymptomsDataNumeric)] = as.numeric(substrRight(unlist(SymptomsData[,1:ncol(SymptomsDataNumeric)]),1)) 
allSymptoms = rowSums(SymptomsDataNumeric) # Add all values together per participant to get a total 'Symptoms' score
DataFrameClean <- cbind(DataFrameClean, allSymptoms) # Add to clean dataframe

# PSST - Disturbance subscale
DisturbanceData <- DataFrame[ , grepl( "Disturbance.PST" , names( DataFrame ) ) ] # Make dataset with only Disturbance variables

# Create dataframe with all individual responses
DisturbanceDataNumeric <- DisturbanceData
DisturbanceDataNumeric[,1:ncol(DisturbanceDataNumeric)] = as.numeric(substrRight(unlist(DisturbanceData[,1:ncol(DisturbanceDataNumeric)]),1)) 
allDisturbance = rowSums(DisturbanceDataNumeric) # Add all values together per participant to get a total 'Disturbance' score
DataFrameClean <- cbind(DataFrameClean, allDisturbance) # Add to clean dataframe

#### RRS ####
RRSData <- DataFrame[ , grepl( "RRS.R" , names( DataFrame ) ) ] # Make dataset with only RRS variables

RRSDataNumeric <- RRSData
RRSDataNumeric[,1:ncol(RRSDataNumeric)] = as.numeric(substrRight(unlist(RRSData[,1:ncol(RRSDataNumeric)]),1)) # Create dataframe with all responses per participant
allRRS = rowSums(RRSDataNumeric) # Add all values together per participant to get a total 'RRS' score

DataFrameClean <- cbind(DataFrameClean, allRRS)

#### DASS #### 
DASSData <- data.frame(DASS.Total = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), DASS.Stress = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), DASS.Anxiety = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), DASS.Depresh = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
DASSDataframe <- DataFrame[ , grepl( "DASS21.DAS" , names( DataFrame ) ) ] # Make dataset with only DASS variables

for(i in 1:nrow(DataFrame)) { # loop through participants
  DASSScore <- 0
  DASSStress <- 0
  DASSAnxiety <- 0
  DASSDepresh <- 0
  for(t in 1:ncol(DASSDataframe)){ # loop through questions
    temp = as.numeric(substrRight(unlist(DASSDataframe[t])[i],1)) # Take value i (participant) from DisturbanceDATA, unlist, then take last character and turn it into a number (double)
    DASSScore <- DASSScore + temp
    if (t == 1 | t == 6 | t == 8 | t == 11 | t == 12 | t == 14 | t == 18){
      DASSStress <- DASSStress + temp
    } else if (t == 2 | t == 4 | t == 7 | t == 9 | t == 15 | t == 19 | t == 20) {
      DASSAnxiety <- DASSAnxiety + temp
    } else if (t == 3 | t == 5 | t == 10 | t == 13 | t == 16 | t == 17 | t == 21){
      DASSDepresh <- DASSDepresh + temp
    }
  }
  DASSData$DASS.Total[i] <- DASSScore
  DASSData$DASS.Stress[i] <- DASSStress
  DASSData$DASS.Anxiety[i] <- DASSAnxiety
  DASSData$DASS.Depresh[i] <- DASSDepresh
}

DataFrameClean <- cbind(DataFrameClean, DASSData)

###### PMS scores (based on PSST) ######
# Use earlier made dataframes: SymptomsDataNumeric & DisturbanceDataNumeric
PMSScore <- 0 #initialize variable
# PMS score is based on conditionals - see PSST - which if writter out in detail below
for (i in 1:nrow(DataFrame)){ #loop over all participants
  req1 = 0 #initialize variables
  req2 = 0
  req3 = 0
  # Requirement 1
  if (sum(SymptomsDataNumeric[i,1:4] == 4)>0){ #if there's one or more values within the first four columns(=questions) of SymptomsDataNumeric that are equal to 4
    req1 = 2  #then requirement 1 gets value 2
  } else if (sum(SymptomsDataNumeric[i,1:4] == 3)>0){ #if there's one or more values within the first four columns of SymptomsDataNumeric that are equal to 3
    req1 = 1 #then requirement 1 gets value 1
  } else { #if there's no values within the first four columns that are equal to 3 or more
    req1 = 0 #then requirement 1 gets value zero
  }
  
  # Requirement 2
  if (sum(SymptomsDataNumeric[i,] >= 3) >= 5){ #if there are 5 or more values in SymptomsDataNumeric that are equal to three or more
    req2 = 2 #then requirement 2 gets value 2
  } else { #if not,
    req2 = 0 #requirement 2 gets value 0
  }
  
  # Requirement 3
  if (sum(DisturbanceDataNumeric[i,] == 4)>0){ #if there's one or more values within the five columns(=questions) of DisturbanceDataNumeric that are equal to 4
    req3 = 2 #then requirement 3 gets value 2
  } else if (sum(DisturbanceDataNumeric[i,] >= 3)>0){ #if there's one or more values within the first four columns of DisturbanceDataNumeric that are equal to 3
    req3 = 1 #then requirement 3 gets value 1 
  } else { #if there's not one value within these five columns that is equal to 3 or more
    req3 = 0 #then requirement 3 gets value 0
  }
  
  #Give each participant a PMSScore
  if (req1 == 2 && req2 == 2 && req3 == 2){ #if the value of req 1 =2, req 2 = 2 and req 3 = 2
    PMSScore[i] <- 2 #then that participant gets PMSScore 2 --> PMDD
  } else if (req1 == 0 | req2 == 0 | req3 == 0){ #if the value of one of the requirements is equal to 0
    PMSScore[i] <- 0 #then that participant gets PMSScore 0 --> no PMS
  } else { #in all other cases...
    PMSScore[i] <- 1 #...the participant gets PMSScore 1 --> PMS
  }
}

PMSData <- cbind(PMSScore, DataFrame) #Add columns with PMSScore to DataFrame

DataFrameClean <- cbind(DataFrameClean, PMSScore) # Add PMS score to clean dataframe

###### CONTRACEPTION ######
ContraData <- data.frame(PMSData$Contraceptive.SQ001., PMSData$Contraceptive.SQ002., PMSData$Contraceptive.SQ003., PMSData$Contraceptive.SQ004., PMSData$Contraceptive.other.) #make dataframe with 'contraceptive' columns only
names(ContraData) <- c("pill", "hor. coil", "cop. coil", "natural", "other") #give new names to these columns

# Compile multiple conditional columns into one containing all information
ContraData$Overview[ContraData$`pill` == 'Y'] = 'Pill'
ContraData$Overview[ContraData$`hor. coil` == 'Y'] = 'Hor. Coil'
ContraData$Overview[ContraData$`cop. coil` == 'Y'] = 'Cop. Coil'
ContraData$Overview[ContraData$`natural` == 'Y'] = 'Natural'
ContraData$Overview[ContraData$other != ''] = 'other'
ContraData$Overview <- as.factor(ContraData$Overview) # Factorize this variable

DataFrameClean <- cbind(DataFrameClean, Contraception = ContraData$Overview) # Add to clean dataframe

# Some people responded something else than 'other' when using Nuvaring, so should be set to other (however it seems like they are already set to 'other' so probably changed in Excel file)
DataFrameClean$Contraception[DataFrame$EMail == 'axxxboels@gmail.com'] = 'other' # axxxboels@gmail.com
DataFrameClean$Contraception[DataFrame$EMail == 'lorerobeyns@hotmail.com'] = 'other' # lorerobeyns@hotmail.com

######## Everything related to moment specific data ######## 
# Load data in (moment A-B or 1-2)
dataMoment1 <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"results-survey10001.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
dataMoment2 <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"results-survey10002.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))

# Clean up data for faulty entries
dataMoment1 <- dataMoment1[!(dataMoment1$lastpage < 3 | is.na(dataMoment1$lastpage)), ]
dataMoment2 <- dataMoment2[!(dataMoment2$lastpage < 3 | is.na(dataMoment2$lastpage)), ]
rownames(dataMoment1) <- NULL
rownames(dataMoment2) <- NULL

# Compute questionnaire scores - functions declared in functions.R
dataMoment1$PSS = getPSS(dataMoment1)
dataMoment1$BSRI = getBSRI(dataMoment1)
dataMoment1$PTQ = getPTQ(dataMoment1)

dataMoment2$PSS = getPSS(dataMoment2)
dataMoment2$BSRI = getBSRI(dataMoment2)
dataMoment2$PTQ = getPTQ(dataMoment2)

##### Link correct questionnaires to participant (based on testing order) ####
# Create empty dataframes with correct size to store all individual responses per item
PSSitems <- data.frame(PSS1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 10), PSS2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 10))
BSRIitems <- data.frame(BSRI1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 8), BSRI2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 8))
PTQitems <- data.frame(PTQ1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 15), PTQ2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 15))
# Create empty dataframes with correct size for total scores
PSS <- data.frame(PSS1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), PSS2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
BSRI <- data.frame(BSRI1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), BSRI2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
PTQ <- data.frame(PTQ1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), PTQ2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
temp = 0 # Counter variable

for (i in 1:nrow(DataFrameClean)){ # Loop over all participant rows that filled out screening completely
  # DataMoment1
  loc = which(dataMoment1$ParticipantNo == DataFrameClean$participantNo[i]) # Check at what location every specific participantNumber is present
  if (length(loc) == 0) {
    temp = temp + 1
    # print(paste0("Something going on with participant ",toString(DataFrameClean$participantNo[i]))) # Check for irregularities
  } else if (length(loc) == 1) {
    PSS$PSS1[i] <- dataMoment1$PSS[loc]
    BSRI$BSRI1[i] <- dataMoment1$BSRI[loc]
    PTQ$PTQ1[i] <- dataMoment1$PTQ[loc]
    # save individual items
    PSSitems[i,1:10] <- as.numeric(substrRight(unlist(dataMoment1[ , grepl("PSS.P", names(dataMoment1))][loc,1:10]),1)) -1
    BSRIitems[i,1:8] <- as.numeric(unlist(dataMoment1[ , grepl("BSRI.B", names(dataMoment1))][loc,1:8]))
    PTQitems[i,1:15] <- as.numeric(substrRight(unlist(dataMoment1[ , grepl("PTQ.P", names(dataMoment1))][loc,1:15]),1)) -1
  } else { # this else is obsolete, everything could be loc[length(loc)], but for completeness sake
    # If there are multiple entries for one participant, we take the last entry #check this later #@Mitchel get back here some time
    PSS$PSS1[i] <- dataMoment1$PSS[loc[length(loc)]]
    BSRI$BSRI1[i] <- dataMoment1$BSRI[loc[length(loc)]]
    PTQ$PTQ1[i] <- dataMoment1$PTQ[loc[length(loc)]]
    # save individual items
    PSSitems[i,1:10] <- as.numeric(substrRight(unlist(dataMoment1[ , grepl("PSS.P", names(dataMoment1))][loc[length(loc)],1:10]),1)) -1
    BSRIitems[i,1:8] <- as.numeric(unlist(dataMoment1[ , grepl("BSRI.B", names(dataMoment1))][loc[length(loc)],1:8]))
    PTQitems[i,1:15] <- as.numeric(substrRight(unlist(dataMoment1[ , grepl("PTQ.P", names(dataMoment1))][loc[length(loc)],1:15]),1)) -1
  }
  # DataMoment2
  loc = which(dataMoment2$ParticipantNo == DataFrameClean$participantNo[i]) # Check at what location every specific participantNumber is present
  if (length(loc) == 0) {
    # print(paste0("Something going on with participant ",toString(DataFrameClean$participantNo[i]))) # Check for irregularities
  } else if (length(loc) == 1) {
    PSS$PSS2[i] <- dataMoment2$PSS[loc]
    BSRI$BSRI2[i] <- dataMoment2$BSRI[loc]
    PTQ$PTQ2[i] <- dataMoment2$PTQ[loc]
    # save individual items
    PSSitems[i,11:20] <- as.numeric(substrRight(unlist(dataMoment2[ , grepl("PSS.P", names(dataMoment2))][loc,1:10]),1)) -1
    BSRIitems[i,9:16] <- as.numeric(unlist(dataMoment2[ , grepl("BSRI.B", names(dataMoment2))][loc,1:8]))
    PTQitems[i,16:30] <- as.numeric(substrRight(unlist(dataMoment2[ , grepl("PTQ.P", names(dataMoment2))][loc,1:15]),1)) -1
  } else {
    # If there are multiple entries for one participant, we take the last entry #check this later #@Mitchel get back here some time
    PSS$PSS2[i] <- dataMoment1$PSS[loc[length(loc)]]
    BSRI$BSRI2[i] <- dataMoment1$BSRI[loc[length(loc)]]
    PTQ$PTQ2[i] <- dataMoment1$PTQ[loc[length(loc)]]
    # save individual items
    PSSitems[i,11:20] <- as.numeric(substrRight(unlist(dataMoment2[ , grepl("PSS.P", names(dataMoment2))][loc[length(loc)],1:10]),1)) -1
    BSRIitems[i,9:16] <- as.numeric(unlist(dataMoment2[ , grepl("BSRI.B", names(dataMoment2))][loc[length(loc)],1:8]))
    PTQitems[i,16:30] <- as.numeric(substrRight(unlist(dataMoment2[ , grepl("PTQ.P", names(dataMoment2))][loc[length(loc)],1:15]),1)) -1
  }
}
# Reverse score individual items where needed for both moments
PSSitems$PSS1.4 = 4 - PSSitems$PSS1.4
PSSitems$PSS1.5 = 4 - PSSitems$PSS1.5
PSSitems$PSS1.7 = 4 - PSSitems$PSS1.7
PSSitems$PSS1.8 = 4 - PSSitems$PSS1.8

PSSitems$PSS2.4 = 4 - PSSitems$PSS2.4
PSSitems$PSS2.5 = 4 - PSSitems$PSS2.5
PSSitems$PSS2.7 = 4 - PSSitems$PSS2.7
PSSitems$PSS2.8 = 4 - PSSitems$PSS2.8

# Declare empty target columns in clean dataframe
DataFrameClean$folliculairPSS = ''
DataFrameClean$folliculairBSRI = ''
DataFrameClean$folliculairPTQ = ''
DataFrameClean$luteaalPSS = ''
DataFrameClean$luteaalBSRI = ''
DataFrameClean$luteaalPTQ = ''
rownames(DataFrameClean) <- NULL # Easier for debugging

DataFrameClean$Order[DataFrameClean$Order == ""] = 'xx' # where no order is present, put 'xx' easier for debugging

##### Create Extensive Dataframe [with individual responses per item] #####
# a dataframe that also contains all individual items for completeness purposes [and cronbach alpha calculations]
DataFrameExtensive <- DataFrameClean
#### Trait questionnaires ####
# PMS Symptoms
DataFrameExtensive <- cbind(DataFrameExtensive, SymptomsDataNumeric)
# PMS Disturbance
DataFrameExtensive <- cbind(DataFrameExtensive, DisturbanceDataNumeric)
# RRS
DataFrameExtensive <- cbind(DataFrameExtensive, RRSDataNumeric)
# DASS
DASSDataNumeric <- DASSDataframe
DASSDataNumeric[,1:ncol(DASSDataNumeric)] = as.numeric(substrRight(unlist(DASSDataframe[,1:ncol(DASSDataNumeric)]),1))
DataFrameExtensive <- cbind(DataFrameExtensive, DASSDataNumeric)

#### State questionnaires #####
# PSS BSRI and PTQ happen in next forloop - but predeclare for correct naming as well
DataFrameExtensive <- cbind(DataFrameExtensive, data.frame(PSS_folliculair = matrix(NA, nrow = nrow(DataFrameClean), ncol = 10), PSS_luteaal = matrix(NA, nrow = nrow(DataFrameClean), ncol = 10)))
DataFrameExtensive <- cbind(DataFrameExtensive, data.frame(BSRI_folliculair = matrix(NA, nrow = nrow(DataFrameClean), ncol = 8), BSRI_luteaal = matrix(NA, nrow = nrow(DataFrameClean), ncol = 8)))
DataFrameExtensive <- cbind(DataFrameExtensive, data.frame(PTQ_folliculair = matrix(NA, nrow = nrow(DataFrameClean), ncol = 15), PTQ_luteaal = matrix(NA, nrow = nrow(DataFrameClean), ncol = 15)))
# dataframebackup <- DataFrameExtensive
# Add the data to the dataFrame for right spot
for (i in 1:nrow(DataFrameClean)){
# for (i in 10:30){
  if (DataFrameClean$Order[i] == "A-B"){
    DataFrameClean$folliculairPSS[i] = PSS$PSS1[i]
    DataFrameClean$folliculairBSRI[i] = BSRI$BSRI1[i]
    DataFrameClean$folliculairPTQ[i] = PTQ$PTQ1[i]
    
    DataFrameClean$luteaalPSS[i] = PSS$PSS2[i]
    DataFrameClean$luteaalBSRI[i] = BSRI$BSRI2[i]
    DataFrameClean$luteaalPTQ[i] = PTQ$PTQ2[i]
    
    # individual items
    DataFrameExtensive[i,1:155] <- cbind(DataFrameExtensive[i,1:89], PSSitems[i,1:10], PSSitems[i,11:20], BSRIitems[i,1:8], BSRIitems[i,9:16], PTQitems[i,1:15], PTQitems[i,16:30])
  } else if (DataFrameClean$Order[i] == "B-A"){
    DataFrameClean$folliculairPSS[i] = PSS$PSS2[i]
    DataFrameClean$folliculairBSRI[i] = BSRI$BSRI2[i]
    DataFrameClean$folliculairPTQ[i] = PTQ$PTQ2[i]
    
    DataFrameClean$luteaalPSS[i] = PSS$PSS1[i]
    DataFrameClean$luteaalBSRI[i] = BSRI$BSRI1[i]
    DataFrameClean$luteaalPTQ[i] = PTQ$PTQ1[i]
    # individual items
    DataFrameExtensive[i,1:155] <- cbind(DataFrameExtensive[i,1:89], PSSitems[i,11:20], PSSitems[i,1:10], BSRIitems[i,9:16], BSRIitems[i,1:8], PTQitems[i,16:30], PTQitems[i,1:15])
    
  } else if (DataFrameClean$Order[i] == 'xx') { # For some reason doesn't have an order assigned yet - so give NA's
    DataFrameClean$folliculairPSS[i] = NA
    DataFrameClean$folliculairBSRI[i] = NA
    DataFrameClean$folliculairPTQ[i] = NA
    
    DataFrameClean$luteaalPSS[i] = NA
    DataFrameClean$luteaalBSRI[i] = NA
    DataFrameClean$luteaalPTQ[i] = NA
    
  } else { # Checks for non-sensical order assignments
    print("Order error")
    break
  }
}
# Now add to extensive dataframe
DataFrameExtensive$folliculairPSS <- DataFrameClean$folliculairPSS
DataFrameExtensive$folliculairBSRI <- DataFrameClean$folliculairBSRI
DataFrameExtensive$folliculairPTQ <- DataFrameClean$folliculairPTQ
DataFrameExtensive$luteaalPSS <- DataFrameClean$luteaalPSS
DataFrameExtensive$luteaalBSRI <- DataFrameClean$luteaalBSRI
DataFrameExtensive$luteaalPTQ <- DataFrameClean$luteaalPTQ

###### Write CSV files ######
write.csv(DataFrameClean, paste0(dataDir,dateDir,"cleanData.csv"), row.names = FALSE)
write.csv(DataFrameExtensive, paste0(dataDir,dateDir,"cleanData_allItems.csv"), row.names = FALSE)

###### Make individual CSV files for state and trait for actual analysis without any noise in the CSV files ######
# Long format using moment (foll vs lut)
msData <- DataFrameClean[is.na(DataFrameClean$folliculairPSS) == FALSE & is.na(DataFrameClean$luteaalPSS) == FALSE, ]
msData <- msData[msData$participantNo != 407, ] # This is a double entry. 

groupingVars <- colnames(msData)[1:24]

msData <- reshape(msData, direction='long', 
                  # varying=c('folliculairPSS', 'folliculairBSRI', 'luteaalPSS', 'luteaalBSRI'), 
                  # varying=c('luteaalPSS', 'luteaalBSRI', 'folliculairPSS', 'folliculairBSRI'), 
                  varying=list(c('folliculairPSS', 'luteaalPSS'),
                               c('folliculairBSRI', 'luteaalBSRI'),
                               c('folliculairPTQ', 'luteaalPTQ')),
                  timevar='Moment',
                  times=c('Foll', 'Lut'),
                  v.names=c('PSS', 'BSRI', 'PTQ'),
                  idvar='participantNo')
colnames(msData)[1] <- 'ID'
colnames(msData)[which(colnames(msData) == "DASS.Stress")] = "DASS_Stress"
colnames(msData)[which(colnames(msData) == "DASS.Anxiety")] = "DASS_Anxiety"
colnames(msData)[which(colnames(msData) == "DASS.Depresh")] = "DASS_Depression"

write.csv(msData, paste0(dataDir,dateDir,"cleanedDataMoments.csv"), row.names = FALSE)

## Wide data for the trait questionnaires
msData <- DataFrameClean
msData <- msData[msData$participantNo != 407, ] # This is a double entry. 
colnames(msData)[1] <- 'ID'
colnames(msData)[which(colnames(msData) == "DASS.Stress")] = "DASS_Stress"
colnames(msData)[which(colnames(msData) == "DASS.Anxiety")] = "DASS_Anxiety"
colnames(msData)[which(colnames(msData) == "DASS.Depresh")] = "DASS_Depression"

write.csv(msData, paste0(dataDir,dateDir,"cleanedDataTraits.csv"), row.names = FALSE)