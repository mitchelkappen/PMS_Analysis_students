rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location

Dir="Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\"

data <- read.csv(paste0(Dir,"06102021\\results-survey987313.csv"), header=TRUE, sep=) #upload data

dataset <- cbind(subset(data, select = c(Age, FirstMenstrual, MenstrualDuration, DASS21.DAS03., DASS21.DAS05., DASS21.DAS10., DASS21.DAS13., DASS21.DAS16., DASS21.DAS17., DASS21.DAS21.)), data[ , grepl("Symptoms.P", names(data))], data[ , grepl("Disturbance.P", names(data))])

dataset <- dataset[dataset$DASS21.DAS03. != "", ] # Get rid of all the incompletes

substrRight <- function(x, n){ # A function that takes the last n characters of a string
  substr(x, nchar(x)-n+1, nchar(x))}

for(i in 1:nrow(dataset)) { # loop through participants
  for(t in 4:ncol(dataset)){ # loop through questions
    temp = as.numeric(substrRight(unlist(dataset[t])[i],1)) # Take value i (participant) from SymptomsDATA, unlist, then take last character and turn it into a number (double)
    # print(dataset[t])[i])
    # print(temp)
    dataset[i,t] = temp
  }
  
}

write.csv(dataset, paste0(Dir,"06102021\\KristofPMSData.csv"), row.names = FALSE)
