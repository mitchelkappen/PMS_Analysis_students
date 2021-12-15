##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(yarrr)
library(lme4)
library(emmeans)
library(pander)

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
# General settings
nAGQ = 1 # When writing code, set to 0, when getting final results, set to 1ù
vpn = 1 # Set to 1 if using VPN

# Set WD
if (vpn == 1) {
  Dir = "Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\" #data from VPN folder
} else {
  Dir = "Z:\\shares\\ghepmk_data\\2020_Kappen_PMS\\" #data from github dir
}

setwd(Dir)
# Get data
data <-
  read.csv(paste0(Dir, "06102021\\cleanedData.csv"),
           header = TRUE,
           sep = ) #upload data

##### Clean data up a bit #####
#we make a new variable that has value 1 for the first TestMoment and 2 for the second TestMoment
#These moments were counterbalanced. when the order was B-A and the moment is B, this meanheas it is the first test moment, and vice versa for A-B and moment A.
data$TestMoment[data$Order == "A-B" &
                  data$Moment == "A"] = 1# TestMoment 1 == Follicular phase
data$TestMoment[data$Order == "B-A" &
                  data$Moment == "A"] = 2# TestMoment 2 == Luteal phase
data$TestMoment[data$Order == "A-B" & data$Moment == "B"] = 2
data$TestMoment[data$Order == "B-A" & data$Moment == "B"] = 1

# new variable PMSSCORE NEW iedereen pms 0 ook 0 iedereen die 1 OF 2 heeft wordt 1,
data$PMS[data$PMSScore == 0] = 'noPMS'
data$PMS[data$PMSScore == 1] = 'PMS'
data$PMS[data$PMSScore == 2] = 'PMDD'

data$PMS <-
  ordered(data$PMS, levels = c('noPMS', 'PMS', 'PMDD')) # Factorize and turn into ordered levels

# Factorize the rest of the data where needed
data$ID <- factor(data$ID)
levs <- union(data$ID, data$ID)
data$newid <-
  factor(data$ID, levels = levs, labels = seq_along(levs)) #this code replaces the '627, 534 IDs with 1, 2, 3, ) || Looks cleaner I guess?

data$PMSScore <- factor(data$PMSScore)
data$PMS <- factor(data$PMS)
data$Moment <-
  factor(data$Moment) # This removes "A and B", A == 1, B == 2 now
data$TestMoment <- factor(data$TestMoment)

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

##### Get and declare functions #####
source("functions.R") # This is a file in the same directory where you can stash your functions so you can save them there and have them together

##### ##### Statistics Time ##### ##### 
dataHalf <- data[!duplicated(data$ID),] # Since we're first looking at trait questionnaires, we only need 1 measure per participant
dataHalf <- data
# dataHalf$DASS_Depression <- rescalepostive(dataHalf$DASS_Depression)
dataHalf$DASS_Depression <- dataHalf$DASS_Depression * 20

##### DASS ##### 
##### DASS: Depression ##### 
formulas= c('DASS_Depression ~ PMS', 'DASS_Anxiety ~ PMS','DASS_Stress ~ PMS' )

# formula <- 'DASS_Depression ~ Age * FirstMenstrual * PMS + (1|ID)'
formula <- 'DASS_Depression ~ PMS + Age + FirstMenstrual + (1|newid)'

# rm(d0.1, d0.2, d0.3, d0.4, d0.5, d0.6, d0.7, d0.8, d0.9)

d0.1 <- lmer(formula,data=dataHalf)
d0.2 <- glmer(formula,data=dataHalf, family = gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataHalf, family = gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

d0.4 <- glmer(formula,data=dataHalf, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.5 <- glmer(formula,data=dataHalf, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.6 <- glmer(formula,data=dataHalf, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

d0.7 <- glmer(formula,data=dataHalf, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.8 <- glmer(formula,data=dataHalf, family = inverse.gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.9 <- glmer(formula,data=dataHalf, family = inverse.gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)


##### States ##### 
##### State: PSS ##### 
formula <- 'PSS ~ PMS * Moment + (1|newid)'

# dataModel <- data[data$PSS != 0, ]
dataModel = data
# rm(d0.1, d0.2, d0.3, d0.4, d0.5, d0.6, d0.7, d0.8, d0.9)

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

d0.4 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.5 <- glmer(formula,data=dataModel, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.6 <- glmer(formula,data=dataModel, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

d0.7 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.8 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.9 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

Anova(d0.1)
plot(effect("PMS", d0.1)) #just to check
plot(effect("PMS:Moment", d0.1)) #just to check

emmeans0.1 <- emmeans(d0.1, pairwise ~ PMS | Moment, adjust ="fdr", type = "response") # Compute a variable containing all emmeans/contrasts
emm0.1 <- summary(emmeans0.1)$emmeans

pd <- position_dodge(0.05) # move them .05 to the left and right
## LINEPLOT
ggplot(emm0.1, aes(x=Moment, y=emmean, color=PMS)) +
  geom_point(size = 1) + 
  geom_line(aes(group = PMS),size = 1)+
  geom_errorbar(width=.125, aes(ymin=emmean-SE, ymax=emmean+SE), position=pd)+
  theme_bw(base_size = 8)+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  ggtitle("Feedback: Group x IBI")+
  labs(y = "Delta IBI (s)")
  # annotate(geom="text", x=xplotPosition, y=-37.5, label="**", color="#000000")+ #IBI3
  # annotate(geom="text", x=xplotPosition + 1, y=-34.5, label="**", color="#000000")+ #IBI4
  # annotate(geom="text", x=xplotPosition + 2, y=-26, label="***", color="#000000")+ #IBI5
  # annotate(geom="text", x=xplotPosition + 3, y=-17, label="***", color="#000000")+ #IBI6
  # annotate(geom="text", x=xplotPosition + 4, y=-12.5, label="**", color="#000000") #IBI7

print('Not a significant interaction effect, but groups among each other show PMS vs noPMs sig. but PMS vs PMDD non-significant')

# Now the other way around, over time
emmeans(d0.1, pairwise ~ Moment | PMS, adjust ="fdr", type = "response")$contrasts

print('Never an effect over time/moment, not as main effect and also not contrasts')

##### State: BSRI ##### 
formula <- 'BSRI ~ PMS * Moment + (1|newid)'

dataModel <- data[data$BSRI != 0, ] # There is only 1 '0' value, so kick it out to enable better fitting models
# dataModel = data
rm(d0.1, d0.2, d0.3, d0.4, d0.5, d0.6, d0.7, d0.8, d0.9)

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

d0.4 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.5 <- glmer(formula,data=dataModel, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.6 <- glmer(formula,data=dataModel, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

d0.7 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.8 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.9 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1,d0.2,d0.3,d0.4,d0.5,d0.6,d0.7,d0.8,d0.9)

# Model Selection
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3), AIC(d0.4), AIC(d0.5), AIC(d0.6), AIC(d0.7), AIC(d0.8), AIC(d0.9))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]]) # Run Anova, double square brackets because of list properties
plot(effect("PMS", chosenModel[[1]])) #just to check
plot(effect("PMS:Moment", d0.1)) #just to check

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ PMS | Moment, adjust ="fdr", type = "response") # Compute a variable containing all emmeans/contrasts
emm0.1 <- summary(emmeans0.1)$emmeans

pd <- position_dodge(0.05) # move them .05 to the left and right
## LINEPLOT
ggplot(emm0.1, aes(x=Moment, y=emmean, color=PMS)) +
  geom_point(size = 1) + 
  geom_line(aes(group = PMS),size = 1)+
  geom_errorbar(width=.125, aes(ymin=emmean-SE, ymax=emmean+SE), position=pd)+
  theme_bw(base_size = 8)+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  ggtitle("Moment x PMS group")+
  labs(y = "BSRI")

print('Significant interaction effect. Groups among each other show PMS vs noPMs sig. but PMS vs PMDD non-significant')

# Now the other way around, over time
emmeans(chosenModel[[1]], pairwise ~ Moment | PMS, adjust ="fdr", type = "response")$contrasts

print('Only significant effect over time for PMS group')




















for(i in 1) {
  Formula <- paste0(Formulas[i], '+ (1|ID)')
  m <- c() # Create empty list to add succesful models to
  tryCatch({ d0.1 <- lmer(Formula,data=data); # if this formula works
  m <- c(m, d1=d0.1)}, #we add this
  error=function(e){})
  tryCatch({ d0.2 <- glmer(Formula,data=data, family = gaussian(link = "inverse"),mustart=pmax(data$PSS, 1e-3),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ); 
  m <- c(m, d2=d0.2)}, error=function(e){})
  tryCatch({ d0.3 <- glmer(Formula,data=data, family = gaussian(link = "log"),mustart=pmax(data$PSS, 1e-3),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ); 
  m <- c(m, d3=d0.3)}, error=function(e){})
  tryCatch({ d0.4 <- glmer(Formula,data=data, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ); 
  m <- c(m, d4=d0.4)}, error=function(e){})
  tryCatch({ d0.5 <- glmer(Formula,data=data, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ); 
  m <- c(m, d5=d0.5) }, error=function(e){})
  tryCatch({ d0.6 <- glmer(Formula,data=data, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ);
  m <- c(m, d6=d0.6) }, error=function(e){})
  tryCatch({ d0.7 <- glmer(Formula,data=data, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ);
  m <- c(m, d7=d0.7) }, error=function(e){})
  tryCatch({ d0.8 <- glmer(Formula,data=data, family = inverse.gaussian(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ); 
  m <- c(m, d8=d0.8) }, error=function(e){})
  tryCatch({ d0.9 <- glmer(Formula,data=data, family = inverse.gaussian(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ=nAGQ); 
  m <- c(m, d9=d0.9) }, error=function(e){})
  m

  
    modelNames<-c()
    tabel<-c()
    chosenModel<-c()
    for (i in 1:length(m)){
      modelNames<-c(modelNames, names(models()[i]))
      AIC<-AIC(models()[[i]])
      tabel <- c (tabel, AIC)}
    chosenModel = modelNames[which(tabel == min(tabel))]
    tabel <- data.frame(Models=c('chosen  Model:', modelNames), AIC= c(chosenModel, round(tabel)))
    tabel

  
  
  
  
  
}










