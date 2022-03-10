#############################
#                           #
##  Function corresponding ##
#     to PMS study code     #
#############################
# Collection of functions used in preprocessingData.R & stateAnalysis.R & traitAnalysis.R & cronbachAlpha.R
# Author: Mitchel Kappen
# 10-3-2022

library(ggplot2) # figures

##### Functions for preprocessingData.R #####
# A function that takes the last n characters of a string
substrRight <- function(x, n){ 
  substr(x, nchar(x)-n+1, nchar(x))}

# Extracting total questionnaire scores from dataframes 
### PSS ####
getPSS <- function(data) {
  tempData <- data[ , grepl("PSS.P", names(data))] # Make dataset with only PSS variables
  allPSS = 0
  
  for(i in 1:nrow(data)) { # loop through participants
    PSSScore <- 0
    for(t in 1:ncol(tempData)){ # loop through questions
      temp = as.numeric(substrRight(unlist(tempData[t])[i],1)) - 1 # Take value i (participant) from RRSDATA, unlist, then take last character and turn it into a number (double) # And substract one because we use different scales
      if (t==4 | t==5 | t==7 | t==8){
        temp = 4 - temp # reserse score these
      }
      PSSScore <- PSSScore + temp
    }
    allPSS[i] <- PSSScore
  }
  return(allPSS)
}

### BSRI ####
getBSRI <- function(data) {
  tempData <- data[ , grepl("BSRI.B", names(data))] # Make dataset with only BSRI variables
  allBSRI = 0
  
  for(i in 1:nrow(data)) { # loop through participants
    BSRIScore <- 0
    for(t in 1:ncol(tempData)){ # loop through questions
      temp = as.numeric(unlist(tempData[t])[i]) # Take value i (participant) from RRSDATA, unlist, then take last character and turn it into a number (double)
      BSRIScore <- BSRIScore + temp
    }
    allBSRI[i] <- BSRIScore
  }
  return(allBSRI)
}

### PTQ ####
getPTQ <- function(data) {
  tempData <- data[ , grepl("PTQ.P", names(data))] # Make dataset with only PTQ variables
  allPTQ = 0
  
  for(i in 1:nrow(data)) { # loop through participants
    PTQScore <- 0
    for(t in 1:ncol(tempData)){ # loop through questions
      temp = as.numeric(substrRight(unlist(tempData[t])[i],1)) - 1
      PTQScore <- PTQScore + temp
    }
    allPTQ[i] <- PTQScore
  }
  return(allPTQ)
}

##### Visualisations #####
### Violin plot ####
geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim, scale = scale, ...)
    )
  }

GeomFlatViolin <- ggproto(
  "GeomFlatViolin",
  Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data %>%
      group_by(group) %>%
      mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = x,
        xmax = x + width / 2
      )
  },
  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <-
      transform(data,
                xminv = x,
                xmaxv = x + violinwidth * (xmax - x))
    # Make sure it's sorted properly to draw the outline
    newdata <-
      rbind(plyr::arrange(transform(data, x = xminv), y),
            plyr::arrange(transform(data, x = xmaxv), -y))
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    ggplot2:::ggname("geom_flat_violin",
                     GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  draw_key = draw_key_polygon,
  default_aes = aes(
    weight = 1,
    colour = "grey20",
    fill = "white",
    size = 0.5,
    alpha = NA,
    linetype = "solid"
  ),
  required_aes = c("x", "y")
)

### Trait viz ####
traitplot <-function(data, emmean_dataframe, var, title){
  ggplot(data, aes(x = PMS, y = .data[[var]])) +
    geom_flat_violin(aes(fill=PMS),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
    geom_boxplot(aes(x = PMS, y = .data[[var]], fill = PMS), outlier.shape=NA, alpha= .45, width = .1, colour = "black") +
    geom_point(data= emmean_dataframe, aes(x = PMS, y = emmean, fill=PMS), size=4)+
    scale_colour_manual(values = c("blue", "red", "purple"))+
    scale_fill_manual(values = c("blue", 'red', 'purple'),
                      name='',labels=c(paste0('noPMS \n n=', as.character(sum(data$PMS == "noPMS"))), paste0('PMS \n n=', as.character(sum(data$PMS == "PMS"))), paste0('PMDD \n n=', as.character(sum(data$PMS == "PMDD")))))+
    guides(fill = guide_legend(reverse=TRUE))+
    labs(y=title)+
    theme(
      legend.key.size=unit(1.3, 'cm'),
      legend.text=element_text(size=13),
      plot.title = element_text(size=rel(2)),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_line( size=.1, color="#dedede" ),
      axis.text.x=element_text(size=rel(1.5)),
      axis.title.y=element_text(size=rel(1.4)),
      axis.title.x = element_blank())
}

### State Viz #####
stateplot <-function(data, emmean_dataframe, var, title){
  ggplot()+ 
    geom_flat_violin(data= data, aes(x= Moment, y= .data[[var]], fill=PMS),position = position_nudge(x =.3, y = 0), adjust = 1.5, alpha = .5, colour = NA)+ # flat violin distribution, .3 points to the right. alpha=.5 so see-through
    geom_boxplot(data= data, aes(x=Moment, y=.data[[var]], fill=PMS), outlier.shape=NA, alpha=.5, width=.3, colour='black')+ #boxplot, see through, no outline, 
    geom_point(data= emmean_dataframe, aes(x = Moment, y = emmean, fill=PMS), position= position_dodge(0.3), size=4)+ #points representing the emmeans
    scale_fill_manual(values = c("blue", 'red', 'purple'), #colours used in plot, repressent PMDD, PMS and noPMS
                      name='', #legend gets no name
                      labels=c(paste0('noPMS \n n=', as.character(sum(data$PMS == "noPMS")/2)), paste0('PMS \n n=', as.character(sum(data$PMS == "PMS")/2)), paste0('PMDD \n n=',as.character(sum(data$PMS == "PMDD")/2))))+ #labels names
    guides(fill = guide_legend(reverse=TRUE))+ # show labels in different order 
    labs(y=title)+
    scale_x_discrete(labels=c("Follicular", "Luteal"))+
    theme(
      legend.key.size=unit(1.3, 'cm'), # make keys of legend bigger
      legend.text=element_text(size=13), # text legend bigger
      plot.title = element_text(size=rel(2)), # plot title bigger
      panel.border = element_blank(), # no border panel (APA)
      panel.background = element_blank(), #white simple background
      axis.line = element_line(colour = "black"), # axis lines black
      panel.grid.major.y = element_line( size=.1, color="#dedede" ), #slight grey horizontal lines
      axis.text.x=element_text(size=rel(2)), #size x axis title
      axis.text.y=element_text(size=rel(1.3)),
      axis.title.y=element_text(size=rel(1.5)), #size y axis title
      axis.title.x = element_blank()) # leave away extra x title (only 'foll' and 'lut')
}

### Correlation plot ####
overall_corr <- function(PSS, PTQ, x_lab, y_lab) {
  dataframe <- data.frame(PSS, PTQ)
  ggscatter(dataframe, x = "PSS", y = "PTQ",
            add='reg.line', fullrange=TRUE,
            conf.int=TRUE,
            cor.coef=TRUE, cor.method='pearson',
            xlab=x_lab, ylab=y_lab)+
    geom_segment(aes(x = -4, y = -4, xend = 40, yend = 40), size= 1, colour='red')
}

###### Statistics ######
### Cohen's d for traits - between groups ####
# cohens_d_trait <- function (var, group1, group2){
#   group1_mu <-mean(data$var[data$PMS==group1], na.rm=TRUE)
#   group2_f <- as.numeric(as.character(data$var[data$PMS==group2]))
#   return(cohensD(group1_mu, group2_f))
# }

cohens_d_trait <- function (var, group1, group2){
  group1 <- var[data$PMS==group1]
  group2 <- var[data$PMS==group2]
  return(cohensD(group1, group2))
}

### Cohen's d for states - between groups ####
cohens_d_state <- function (var, testmoment, group1, group2){
  group1_mu <- mean(var[data$PMS==group1 & data$Moment==testmoment], na.rm=TRUE)
  group2_f <- as.numeric(as.character(var[data$PMS==group2& data$Moment==testmoment]))
  return(cohensD(group1_mu, group2_f))
}

### Cohen's d for states - between moment ####
cohens_d_moments <- function(var, PMSgroup){
  lut <- mean(var[data$PMS==PMSgroup & data$Moment=='Lut'])
  fol <- as.numeric(as.character(var[data$PMS==PMSgroup& data$Moment=='Foll']))
  return(cohensD(fol, lut))
}

### Phi (effect size for anova) ####
phi_from_chisq <- function(anovatable,  N){
  round(sqrt(anovatable[1] / N),2)
}