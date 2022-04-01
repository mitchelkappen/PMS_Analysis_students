


#### Settings & Packages #### -----------------------------------------------------
rm(list = ls())
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test
library(lme4)
library(lmerTest)
library(effects)
library(dplyr)
library(car)
library(emmeans)
library(fitdistrplus)
library(dplyr)
library(car)
# library(MuMIn)
library(ggplot2)
library(ggstatsplot)
library(ggsignif)
library(ggformula)
library(ggdist)
library(raincloudplots)
library(gridExtra)
library(tidyverse)
library(ggeffects)
library(pander)


library(cowplot)
library(readr)
library(jpeg)
library(ggpubr)

# library(viridis)
library (yarrr)
library(knitr)
recode <- dplyr::recode
# Suppress summarize info
options(dplyr.summarise.inform = FALSE)
count <- dplyr::count 
# set the theme to theme_bw for all ggplot2 figures
theme_set(theme_bw())
# create folder to save figures
if (!dir.exists("figures")) dir.create("figures")


# General settings
nAGQ = 0 # When writing code, set to 0, when getting final results, set to 1

#### IMPORT data & INSPECTION #### -------------------------------------------------------------
knitr::opts_knit$set(root.dir = dirname(rstudioapi::getActiveDocumentContext()$path))# Set working directory to current directory
# setwd("C:\Users\ASUSTeK\OneDrive\2021-2022\internship\projects")
data <- read.csv("Data/allPMSdata.csv", header=TRUE)
# data <- read.table("data/allPMSdata.csv",sep="\t", header=TRUE)


# from wide to long
#check if subject column is a facctor
#str(data)
#head(data)
# data <-as.numeric(data)
data$Subject <- factor(data$ID)
#typeof(data$Subject)


#we make a new variable that has value 1 for the first TestMoment and 2 for the second TestMoment
#These moments were counterbalanced
#when the order was B-A and the moment is B, this means it is the first test moment
#and vice versa for A-B and moment A. 

# TestMoment 1 == Follicular phase
# TestMoment 2 == Luteal phase
data$TestMoment[data$Order == "A-B" & data$Moment == "A"] = 1
data$TestMoment[data$Order == "B-A" & data$Moment == "A"] = 2
data$TestMoment[data$Order == "A-B" & data$Moment == "B"] = 2
data$TestMoment[data$Order == "B-A" & data$Moment == "B"] = 1
#check if there are still values missing (NA)
#sum(is.na(data$TestMoment))

# new variable PMSSCORE NEW iedereen pms 0 ook 0 iedereen die 1 OF 2 heeft wordt 1, 
data$PMSScoreNew[data$PMSScore==0] = 'noPMS'
data$PMSScoreNew[data$PMSScore==1] = 'PMS'
data$PMSScoreNew[data$PMSScore==2] = 'PMS' #PMDD, mr niet officiële diagnose dus gewoon PMS
#sum(is.na(data$PMSScoreNew))


# Check whether R recognizes the variable types correctly
#we make factors of the independable variables

data$PMSScore <- factor(data$PMSScore)
data$PMSScoreNew <- factor(data$PMSScoreNew)

data$Moment <- factor(data$TestMoment) # This removes "A and B", A == 1, B == 2 now

# Exclude data?
dataBig = data # Saved all the data here
data = data[, -which(names(data) == "X" | names(data) == "Stimulus" | names(data) == "Valence" | names(data) == "Arousal" | names(data) == "rt")] #removes these columns
data= distinct(data)

# exclude everyone on the pill/copper spiral/other: only those with Natural Contraception are left included
data<-subset(data, Contraception!="Pill" & Contraception!="other" & Contraception!="Cop. Coil" & Contraception!="Hor.Coil")

data <- subset(data,BSRI!=0 ) #remove datapoints where BSRI = 0

# Define the formula for the model & check which model fits the data best

data_temp <- data #to get back to

  df<- data_1x1(array_1=data$PSS, array_2=data$PMSScoreNew, jit_distance=0.09, jit_seed=321)+
    raincloud_2<- raincloud_1x1_repmes(
      data= df(),
      colors= (c('dodgerblue', 'darkorange')),
      fills= (c('dodgerblue', 'darkorange')),
      line_color="gray",
      line_alpha=.3,
      size=1,
      alpha= .6,
      align_clouds=FALSE)+
      
      # scale_x_cntinuous (breaks=c(1,2), labels=c("1", "2"), limits=c(0,3))+
      # xlab("PMS")+
      # theme_classic()
      
      raincloud_2
