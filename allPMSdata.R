# ---
#   title: "PMS Study"
# author: "Mitchel Kappen & Sofie Raeymakers"
# date: "`r Sys.setlocale('LC_TIME', 'C'); format(Sys.time(), '%d\\\\. %B %Y')`"

# we will first write some pseudocode to describe what we want to do
#upload files
#find best linear model to represent data with AI

#     PSS ~ PMSScore*Moment + (1|Subject)
# 
#     PSS deppendent variablle
#     pMS = 1 geen PMS
#     keer moment: moment v afname
#     1 out of subject: random effect toevoegen
#     straks factoren vd PMS score en van moment: hoeveel levels? 
#       formules van doen
#     laten weten welke formule beste
#     


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
library(gridExtra)
library(tidyverse)
library(ggeffects)
library(pander)
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




#### IMPORT DATA & INSPECTION #### -------------------------------------------------------------
knitr::opts_knit$set(root.dir = dirname(rstudioapi::getActiveDocumentContext()$path))# Set working directory to current directory
# setwd("C:\Users\ASUSTeK\OneDrive\2021-2022\internship\projects")
data <- read.csv("Data/allPMSdata.csv", header=TRUE)
# data <- read.table("Data/allPMSdata.csv",sep="\t", header=TRUE)


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
data$PMSScoreNew[data$PMSScore==2] = 'PMS'
#sum(is.na(data$PMSScoreNew))


# Check whether R recognizes the variable types correctly
#we make factors of the independable variables

data$PMSScore <- factor(data$PMSScore)
data$PMSScoreNew <- factor(data$PMSScoreNew)

data$Moment <- factor(data$TestMoment) # This removes "A and B", A == 1, B == 2 now

# Exclude data?
dataBig = data # Saved all the data here
data = data[, -which(names(data) == "X" | names(data) == "Stimulus" | names(data) == "Valence" | names(data) == "Arousal" | names(data) == "rt")]
data = distinct(data)

# exclude everyone on the pill/copper spiral/other: only those with Natural Contraception are left included
data<-subset(data_uniq, Contraception!="Pill" & Contraception!="other" & Contraception!="Cop. Coil" & Contraception!="Hor.Coil")

# Define the formula for the model & check which model fits the data best
formulas = c('PSS ~ PMSScoreNew*TestMoment') # important! after the ~should be the same every time

plotTitles = c('PMSScore*TestMoment')

data_temp <- data #to get back to

# Define the formula for the model & check which model fits the data best
for (i in 1){
  
  Formula <- paste0(formulas[i], ' + (1|Subject)')
  d0.1 <- lmer(Formula,data=data)
  d0.2 <- glmer(Formula,data=data, family = gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  d0.3 <- glmer(Formula,data=data, family = gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  
  d0.4 <- glmer(Formula,data=data, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  d0.5 <- glmer(Formula,data=data, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  d0.6 <- glmer(Formula,data=data, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  
  d0.7 <- glmer(Formula,data=data, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  d0.8 <- glmer(Formula,data=data, family = inverse.gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  d0.9 <- glmer(Formula,data=data, family = inverse.gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ)
  
  modelNames = c(d0.1,d0.2,d0.3,d0.4,d0.5,d0.6,d0.7,d0.8,d0.9)
  
  # Remove the models that produced errors in the line below, the one with the lowest value is the best fitting model
  tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3), AIC(d0.4), AIC(d0.5), AIC(d0.6), AIC(d0.7), AIC(d0.8), AIC(d0.9))
  #show table in html
  kable (tabel, caption = "table showing the different AIC")
  chosenModel = modelNames[which(tabel == min(tabel))]
  
  
  Anova(chosenModel[[1]])
  #   Analysis of Deviance Table (Type II Wald chisquare tests)
  # 
  # Response: PSS
  #                          Chisq Df Pr(>Chisq)    
  # PMSScoreNew            10.8504  1  0.0009877 ***
  # TestMoment              0.4827  1  0.4872097    
  # PMSScoreNew:TestMoment  0.0272  1  0.8689949    
  # ---
  # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  emmeans(chosenModel[[1]], pairwise ~ PMSScoreNew*TestMoment , adjust ="fdr", type="response")
  #  PMSScoreNew TestMoment emmean    SE  df asymp.LCL asymp.UCL
  #  noPMS                1   29.4 0.289 Inf      28.8      30.0
  #  PMS                  1   30.5 0.306 Inf      29.9      31.1
  #  noPMS                2   29.2 0.300 Inf      28.6      29.8
  #  PMS                  2   30.4 0.327 Inf      29.8      31.1
  # 
  # Confidence level used: 0.95 
  # 
  # $contrasts
  #  contrast          estimate    SE  df z.ratio p.value
  #  noPMS 1 - PMS 1     -1.142 0.421 Inf  -2.713  0.0133 *
  #  noPMS 1 - noPMS 2    0.204 0.325 Inf   0.626  0.6376
  #  noPMS 1 - PMS 2     -1.018 0.436 Inf  -2.334  0.0294 *
  #  PMS 1 - noPMS 2      1.345 0.429 Inf   3.137  0.0102 *
  #  PMS 1 - PMS 2        0.124 0.360 Inf   0.344  0.7311
  #  noPMS 2 - PMS 2     -1.222 0.444 Inf  -2.751  0.0133 *
  # 
  # P value adjustment: fdr method for 6 tests 
  
  # Plotting
  dpi=600    #pixels per square inch
  # jpeg(paste0(plotPrefix, "Figure", "_", plotTitles[i], ".jpeg"), width=8*dpi, height=4*dpi, res=dpi)
  par(mfcol = c(1, 1))
  plot <- pirateplot(
    formula = formulas[i],
    data = data,
    theme = 1,
    pal = "info",
    main = plotTitles[i],
    bean.f.o = .6, # Bean fill
    point.o = .3,  # Points
    inf.f.o = .7,  # Inference fill
    inf.b.o = .8,  # Inference border
    avg.line.o = 1,  # Average line
    # bar.f.o = .5, # Bar
    inf.f.col = "white",  # Inf fill col
    inf.b.col = "black",  # Inf border col
    avg.line.col = "black",  # avg line col
    bar.f.col = gray(.8),  # bar filling color
    point.pch = 21,
    point.bg = "white",
    point.col = "black",
    point.cex = .7,
    
    xlab = "",
  )
  # abline(lm(formulas[i], data=data), lwd=4, lty=2, col = "red")
  dpi=600    #pixels per square inch
  
  # jpeg(paste0(plotPrefix, "Figure", "_", plotTitles[i], ".jpeg"), width=8*dpi, height=4*dpi, res=dpi)
  par(mfcol = c(1, 1))
  plotPSS_1 <- pirateplot(
    formula = formulas[i],
    data = data,
    theme = 3,
    pal = "info",
    main = plotTitles[i],
    bean.f.o = .6, # Bean fill
    point.o = .3,  # Points
    inf.f.o = .7,  # Inference fill
    inf.b.o = .8,  # Inference border
    avg.line.o = 1,  # Average line
    # bar.f.o = .5, # Bar
    inf.f.col = "white",  # Inf fill col
    inf.b.col = "black",  # Inf border col
    avg.line.col = "black",  # avg line col
    bar.f.col = gray(.8),  # bar filling color
    point.pch = 21,
    point.bg = "white",
    point.col = "black",
    point.cex = .7,
    
    xlab = "",
  )
  
  # Insert visual indicators of significance
  x <- 1:2  # capture x coordinates of bars
  y <- 35  # create the y coordinate of the line
  offset <- 0.2  # set an offset for tick lengths
  lines(c(1.2,1.8),c(y, y))  # draw first horizontal line
  lines(c(1.2,1.2),c(y, y-offset))  # draw ticks
  lines(c(1.8,1.8),c(y, y-offset))
  text(x[1]+((x[2]-x[1])/2),y+offset,"***")  # draw asterics
  
  
}






## Plots





ggplot(data = data) + 
  geom_bar(mapping = aes(x = PSS))

##PIRATE PLOT:

# Plotting
dpi=600    #pixels per square inch

# jpeg(paste0(plotPrefix, "Figure", "_", plotTitles[i], ".jpeg"), width=8*dpi, height=4*dpi, res=dpi)
par(mfcol = c(1, 1))
plotPSS_1 <- pirateplot(
  formula = PSS ~ TestMoment,
  data = data,
  theme = 3,
  pal = "info",
  main = "PSS ~ Testmoment",
  bean.f.o = .6, # Bean fill
  point.o = .3,  # Points
  inf.f.o = .7,  # Inference fill
  inf.b.o = .8,  # Inference border
  avg.line.o = 1,  # Average line
  # bar.f.o = .5, # Bar
  inf.f.col = "white",  # Inf fill col
  inf.b.col = "black",  # Inf border col
  avg.line.col = "black",  # avg line col
  bar.f.col = gray(.8),  # bar filling color
  point.pch = 21,
  point.bg = "white",
  point.col = "black",
  point.cex = .7,
  
  xlab = "",
)

# Insert visual indicators of significance
x <- 1:2  # capture x coordinates of bars
y <- 35  # create the y coordinate of the line
offset <- 0.2  # set an offset for tick lengths
lines(c(1.2,1.8),c(y, y))  # draw first horizontal line
lines(c(1.2,1.2),c(y, y-offset))  # draw ticks
lines(c(1.8,1.8),c(y, y-offset))
text(x[1]+((x[2]-x[1])/2),y+offset,"***")  # draw asterics



################################################

plotPSS_2 <- pirateplot(
  formula = PSS ~ PMSScoreNew*TestMoment,
  data = data,
  theme = 3,
  pal = "info",
  main = "PSS ~ PMSScoreNew: Testmoment",
  bean.f.o = .6, # Bean fill
  point.o = .3,  # Points
  inf.f.o = .7,  # Inference fill
  inf.b.o = .8,  # Inference border
  avg.line.o = 1,  # Average line
  # bar.f.o = .5, # Bar
  inf.f.col = "red",  # Inf fill col
  inf.b.col = "black",  # Inf border col
  avg.line.col = "black",  # avg line col
  bar.f.col = grey(.8),  # bar filling color
  point.pch = 21,
  point.bg = "grey",
  point.col = "black",
  point.cex = .7,
  
  xlab = "",
)





ZeroLength<- length(which(data$PMSScoreNew=='noPMS'))
OneLength <-length(which(data$PMSScoreNew=='PMS'))


age_mean <- round(mean(data$Age), 1)

summary(data$PSS)

# count for each participants how many trials are left for each PMSScoreNew by PSS
trial_count <- as.data.frame(table(data$Subject, data$PMSScoreNew, data$PSS))
names(trial_count) <- c("subject_id", "PMSScore", "PSS", "count")

trial_mean <- trial_count %>%
  group_by(PMSScore, PSS) %>%
  summarize(
    mean = mean(count),
    sd = sd(count), 
    min = min(count),
    max = max(count)
  )

```
##we have more participants with low PMS score


outliers <- boxplot(data$PSS, plot=FALSE)$out

Q <- quantile(data$PSS, probs= c(.25, .75), na.rm=TRUE)

iqr <-IQR(data$PSS, na.rm=TRUE)
up <- Q[2]+1.5*iqr

low<- Q[1]-1.5*iqr


#data$PSS_without_out<-
PSS_without_out<- data[-which(data$PSS %in% outliers),]



  
  
  ############ Hyptothesen en onderzoeksvragen:
  
  
  TO DO Woensdag:
  
  re-check Anova en emmeans

PMS vs noPMS op het gebied van:
  - PSS
- BSRI

++ Interactie testmoment


PMS vs noPMS voor:
  - DASS_alle subschalen
-- Hoeft niet x testmoment

=> automatiseer dit: via for loop





------- Voor de stimuli -----------
  Eerste stap:
  - Bekijken Valence en arousal tussen de groepen en testmomenten, over alle stimuli als een geheel

Next:
  - Per stimulus gemiddelden plotten
-- Om erachter te komen of er coherent geantwoord wordt
-- Om te zien of ze niet te veel van elkaar afwijken
--- waarchuwing; ze gaan veel van elkaar afwijken. 


Stimuli onderverdelen in negatief, positief en neutral, om daar een subdivisie analyse op te maken
*noot: iaos images, ze zijn getagged (zoek op om ze te kunnen onderverdelen). 22, gematched (dus bv naald in arm vs naald in been)




---------------- Overig:
  Normgroepen opzoeken voor PSS bij (jonge) vrouwen
https://www.cambridge.org/core/journals/spanish-journal-of-psychology/article/abs/perceived-stress-scale-pss-normative-data-and-factor-structure-for-a-largescale-sample-in-mexico/752B62D15A932A2D148D04376E949F67

https://bmcpsychiatry.biomedcentral.com/articles/10.1186/s12888-016-0875-9/tables/6


