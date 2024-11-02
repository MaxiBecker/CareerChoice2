# R-Script for analyses carried out in manuscript: Oeser, Hartey, Becker &  Siebert (in prep)
# last update 10/10/24

rm(list = ls())
################### 0)  load libraries  #############
library(lme4)
#library(MASS)
library(psych)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sjlabelled)      
library(sjmisc)                                                                                    
library(sjstats)
library(sjPlot)
library(tidyr)
library(ggeffects)
library(performance)
library(emmeans)
library(cowplot)
library(dplyr)
#lbrary(readr)
library(effectsize)
library(predictmeans)# for permutation tests 
source("R_rainclouds.R")

setwd("I:/Meine Ablage/uni/_studies/IN_PREP/KLUG2/data") 
load("Oeser_Hartey_Siebert_Becker_2024.Rdata")

####### 0) useful functions #######
normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

########################################################################-
data$time2 = data$time
data$time_fac = as.factor(data$time2)
data$time_ord = ordered(data$time, levels=c("t1" ,"t2","t3"),
                                   labels=c("t1" ,"t2","t3"))
data$sex_fac = as.factor(data$Sex)
data$group[data$group == "K"] = "C"
data$group[data$group == "E"] = "I"
data$group = as.factor(data$group)

data$time = factor(data$time, levels = c("t1", "t2", "t3"))
data$CCSE = data$berufswahl_SW_sum
data$OSE = data$berufliche_SW_sum 
data$SE = data$Selbstwert_sum
data$GR = data$AllgResilienz_sum

data$gender = data$Sex
data$gender[data$gender == "M"] = 0
data$gender[data$gender == "F"] = 1
data$gender[data$gender == "divers"] = .5
data$gender = as.integer(data$gender)

##### Descriptives of sample re. gender ####################

# compare sex between both groups
KLUG_sex= data[data$time_fac == "t1" , ] %>% group_by(group) %>% count(Sex)
KLUG_sex_per_school= data[data$time_fac == "t1" , ] %>% group_by(group, school) %>% count(Sex)

KLUG_sex_t2= data[data$time_fac == "t2" , ] %>% group_by(group) %>% count(Sex)
KLUG_sex_per_school_t2= data[data$time_fac == "t2" , ] %>% group_by(group, school) %>% count(Sex)

KLUG_sex_t3= data[data$time_fac == "t3" , ] %>% group_by(group) %>% count(Sex)
KLUG_sex_per_school_t3= data[data$time_fac == "t3" , ] %>% group_by(group, school) %>% count(Sex)

  #test, if intervention & control group differ in terms of sex
  gender_table <- table(data[data$time == "t3",]$Sex, data[data$time == "t3",]$group)
  chi_test <- chisq.test(gender_table)
  
###### describe relevant Outcome Variables: 
#Motivation: MOT_Betroffenheit,MOT_Eigenverantwortung, MOT_Offenheit, MOT_Zuversicht, MOT_all_sum
#Ratschinksy: GR, SE, OSE, CCSE

summary(data)

#################################################################-
####### 1) MOTVIATION ######################################

########## 1a) CCM gernal ###### 
MOT_all_bigger =  mean(data$MOT_all_mean)+2.5*sd(data$MOT_all_mean)
MOT_all_smaller =  mean(data$MOT_all_mean)-2.5*sd(data$MOT_all_mean)
new_data = data[!(data$MOT_all_mean< MOT_all_smaller | data$MOT_all_mean> MOT_all_bigger),]

# do model stats
MOT_all_sum_LMER0 <- lmer(MOT_all_mean ~ time_ord+group  + (1|ID) + school,data=new_data, na.action= na.omit)
MOT_all_sum_LMER1 <- lmer(MOT_all_mean ~ time_ord*group  + (1|ID) + school,data=new_data, na.action= na.omit)

  # permutation test
    Mperm_MOT_all_sum_LMER = permlmer(MOT_all_sum_LMER0,MOT_all_sum_LMER1,  seed = 123)
    Mperm_MOT_all_sum_LMER2= permmodels(MOT_all_sum_LMER1, type = 2, seed = 123)
   # effectsize(MOT_all_sum_LMER1)
  
#check distributions of UV & AV 
  plot(check_distribution(MOT_all_sum_LMER1))
  hist(residuals(MOT_all_sum_LMER1))

#summarize results
  MOT_all_sum_LMER_table = tab_model(MOT_all_sum_LMER1, show.std = T, show.est = F)

# get mean values and ci's
  #confint(MOT_all_sum_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(MOT_all_sum_LMER1, list(pairwise ~ time_ord | group ), adjust = "tukey")
  
# plot the results
  VVV = MOT_all_sum_LMER1
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x

  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$MOT_all_mean   #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID

  MOT_all_pic_final= #"#02BCAD"
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .3) +
    geom_jitter( position = position_jitterdodge(jitter.width = .5, dodge.width = .12) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(8,30))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression(CCM), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
   theme(strip.background = element_blank())
  MOT_all_pic_final


###### 1b) CCM - confidence ######## 
MOT_zuv_bigger =  mean(data$MOT_Zuversicht)+2.5*sd(data$MOT_Zuversicht)
MOT_zuv_smaller =  mean(data$MOT_Zuversicht)-2.5*sd(data$MOT_Zuversicht)
new_data = data[!(data$MOT_Zuversicht< MOT_zuv_smaller | data$MOT_Zuversicht> MOT_zuv_bigger),]

MOT_Zuversicht_LMER0 <- lmer(MOT_Zuversicht ~ time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
MOT_Zuversicht_LMER1 <- lmer(MOT_Zuversicht ~ time_ord*group +  (1|ID)+ school,data=new_data, na.action= na.omit)

#check distributions of UV & AV  -> normal
  plot(check_distribution(MOT_Zuversicht_LMER1))
  hist(residuals(MOT_Zuversicht_LMER1))

  # permutation  to get p-values
    Mperm_MOT_zuv_sum_LMER = permlmer(MOT_Zuversicht_LMER0,MOT_Zuversicht_LMER1,  seed = 123)
    Mperm_MOT_zuv_sum_LMER2= permmodels(MOT_Zuversicht_LMER1, type = 2, seed = 123)
  
  #summarize results
  MOT_Zuversicht_LMER_table = tab_model(MOT_Zuversicht_LMER1, show.std =T)
  #effectsize(MOT_Zuversicht_LMER1)
  
# get mean values and ci's
  #confint(MOT_Zuversicht_LMER1, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(MOT_Zuversicht_LMER1, list(pairwise ~ time_ord | group ), adjust = "tukey")
  
# plot the results
  VVV = MOT_Zuversicht_LMER1
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$MOT_Zuversicht   #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID

  MOT_confidence_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here #geom_jitter( position = position_jitterdodge(jitter.width = .3, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(8,52))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression(CCM[confidence]), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  MOT_confidence_pic_final
  
##########################################################################-
###### 1c) CCM - openness #####
  MOT_off_bigger =  mean(data$MOT_Offenheit)+2.5*sd(data$MOT_Offenheit)
  MOT_off_smaller =  mean(data$MOT_Offenheit)-2.5*sd(data$MOT_Offenheit)
  new_data = data[!(data$MOT_Offenheit< MOT_off_smaller | data$MOT_Offenheit> MOT_off_bigger),]

MOT_Offenheit_LMER0 <- lmer(MOT_Offenheit ~ time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
MOT_Offenheit_LMER <- lmer(MOT_Offenheit ~ time_ord*group +  (1|ID)+ school,data=new_data, na.action= na.omit)

  #check distributions of UV & AV  -> normal
    plot(check_distribution(MOT_Offenheit_LMER))
    hist(residuals(MOT_Offenheit_LMER))

  # permutation test
      Mperm_MOT_off_sum_LMER = permlmer(MOT_Offenheit_LMER0,MOT_Offenheit_LMER,  seed = 123)
      Mperm_MOT_off_sum_LMER2= permmodels(MOT_Offenheit_LMER, type = 2, seed = 123)
  
  #summarize results
    MOT_Offenheit_LMER_table = tab_model(MOT_Offenheit_LMER, show.std = T)

  # get mean values and ci's
    #confint(MOT_Offenheit_LMER, method = "boot") #-> bootstrapped CIs but dont work
    #emmeans(MOT_Offenheit_LMER, list(pairwise ~ time_ord | group ), adjust = "tukey")
  
# plot the results
  VVV = MOT_Offenheit_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$MOT_Offenheit   #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  
  MOT_openness_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here #geom_jitter( position = position_jitterdodge(jitter.width = .3, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(0,25))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression(CCM[openness]), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  
  
##########################################-
####### 1d) CCM - personal responsibility  ###### 
  MOT_eig_bigger =  mean(data$MOT_Eigenverantwortung)+2.5*sd(data$MOT_Eigenverantwortung)
  MOT_eig_smaller =  mean(data$MOT_Eigenverantwortung)-2.5*sd(data$MOT_Eigenverantwortung)
  new_data = data[!(data$MOT_Eigenverantwortung< MOT_eig_smaller | data$MOT_Eigenverantwortung> MOT_eig_bigger),]
  
  MOT_Eigenverantwortung_LMER0 <- lmer(MOT_Eigenverantwortung~ time_ord+group +  (1|ID)+ school,data=new_data,  na.action= na.omit)
  MOT_Eigenverantwortung_LMER <- lmer(MOT_Eigenverantwortung ~ time_ord*group +  (1|ID)+ school,data=new_data,  na.action= na.omit)

  #check distributions of UV & AV  -> normal
  plot(check_distribution(MOT_Eigenverantwortung_LMER))
  hist(residuals(MOT_Eigenverantwortung_LMER))
  
  # permutation test
    Mperm_MOT_eig_sum_LMER = permlmer(MOT_Eigenverantwortung_LMER0,MOT_Eigenverantwortung_LMER,  seed = 123)
    Mperm_MOT_eig_sum_LMER2= permmodels(MOT_Eigenverantwortung_LMER, type = 2, seed = 123)

  #summarize results
  MOT_Eigenverantwortung_LMER_table = tab_model(MOT_Eigenverantwortung_LMER,show.std = T)
  #effectsize(MOT_Eigenverantwortung_LMER)
  
  # get mean values and ci's
  #confint(MOT_Eigenverantwortung_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(MOT_Eigenverantwortung_LMER, list(pairwise ~ time_ord | group ), adjust = "tukey")
  
  # plot the results
  VVV = MOT_Eigenverantwortung_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$MOT_Eigenverantwortung   #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  
  MOT_responsibility_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here #geom_jitter( position = position_jitterdodge(jitter.width = .3, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(0,23))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression(CCM[personal_responsibility]), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  
  
###########################################-
####### 1e) CCM - concern #######
  MOT_bet_bigger =  mean(data$MOT_Betroffenheit)+2.5*sd(data$MOT_Betroffenheit)
  MOT_bet_smaller =  mean(data$MOT_Betroffenheit)-2.5*sd(data$MOT_Betroffenheit)
  new_data = data[!(data$MOT_Betroffenheit< MOT_bet_smaller | data$MOT_Betroffenheit> MOT_bet_bigger),]

MOT_Betroffenheit_LMER0 <- lmer(MOT_Betroffenheit~ time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
MOT_Betroffenheit_LMER <- lmer(MOT_Betroffenheit ~ time_ord*group +  (1|ID)+ school,data=new_data, na.action= na.omit)

  #check distributions of UV & AV  -> normal
  plot(check_distribution(MOT_Betroffenheit_LMER))
  hist(residuals(MOT_Betroffenheit_LMER))

  # permutation test
    Mperm_MOT_bet_sum_LMER = permlmer(MOT_Betroffenheit_LMER0,MOT_Betroffenheit_LMER,  seed = 123)
    Mperm_MOT_bet_sum_LMER2= permmodels(MOT_Betroffenheit_LMER, type = 2, seed = 123)

  #summarize results
  MOT_Betroffenheit_LMER_table = tab_model(MOT_Betroffenheit_LMER, show.std=T)
  #summary(MOT_Betroffenheit_LMER, ddf = "Satterthwaite")

  # get mean values and ci's
  #confint(MOT_Betroffenheit_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(MOT_Betroffenheit_LMER, list(pairwise ~ time_ord | group ), adjust = "tukey")
  #effectsize(MOT_Betroffenheit_LMER)
  
  # plot the results
  VVV = MOT_Betroffenheit_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$MOT_Betroffenheit   #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  
  MOT_concern_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here ##geom_jitter( position = position_jitterdodge(jitter.width = .3, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(7,30))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression(CCM[concern]), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  
  
###################################################################-
#### 2) RATSCHINKSKY SCALE ######################################

######################################################################-
######### 2a)  CCSE - carrer choice self-efficacy    ###########################
  bigger =  mean(data$CCSE)+2.5*sd(data$CCSE)
  smaller =  mean(data$CCSE)-2.5*sd(data$CCSE)
  new_data = data[!(data$CCSE< smaller | data$CCSE> bigger),]

CCSE_LMER0 <- lmer(CCSE  ~  time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
CCSE_LMER <- lmer(CCSE  ~  time_ord*group + (1|ID)+ school,data=new_data, na.action= na.omit)

  #check distributions of UV & AV  -> normal
  plot(check_distribution(CCSE_LMER))
  hist(residuals(CCSE_LMER))
  
  # permutation test
    Mperm_CCSE_sum_LMER = permlmer(CCSE_LMER0,CCSE_LMER,  seed = 123)
    Mperm_CCSE_sum_LMER2= permmodels(CCSE_LMER, type = 2, seed = 123)

  #summarize results
  CCSE_LMER_table = tab_model(CCSE_LMER, show.std = T, show.est = F)
  #effectsize(CCSE_LMER)

  # get mean values and ci's
  #confint(CCSE_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(CCSE_LMER, list(pairwise ~ time_ord | group ), adjust = "tukey")
  emmeans(CCSE_LMER, list(pairwise ~ time_ord), adjust = "tukey")
  
  # plot the results
  VVV = CCSE_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$CCSE  #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  
  CCSE_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_jitter( position = position_jitterdodge(jitter.width = .25, dodge.width = .1) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(8,24))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression("CCSE"), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  CCSE_pic_final

######### 2b)  OSE - occupational self-efficacy  ###########################
  bigger =  mean(data$OSE)+2.5*sd(data$OSE)
  smaller =  mean(data$OSE)-2.5*sd(data$OSE)
  new_data = data[!(data$OSE< smaller | data$OSE> bigger),]

OSE_LMER0 <- lmer(OSE ~ time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
OSE_LMER <- lmer(OSE ~ time_ord*group +  (1|ID)+ school,data=new_data, na.action= na.omit)
check_collinearity(OSE_LMER)

  #check distributions of UV & AV  -> normal
  plot(check_distribution(OSE_LMER))
  hist(residuals(OSE_LMER))
  
  # permutation test
    Mperm_OSE_sum_LMER = permlmer(OSE_LMER0,OSE_LMER,  seed = 123)
    Mperm_OSE_sum_LMER2= permmodels(OSE_LMER, type = 2, seed = 123)

  #summarize results
  OSE_LMER_table = tab_model(OSE_LMER, show.std = T)
  emmeans(OSE_LMER, list(pairwise ~ time_ord  ), adjust = "tukey")
  
  
  # get mean values and ci's
  #confint(berufSW_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(OSE_LMER, list(pairwise ~  time_ord   ), adjust = "tukey")
  
  # plot the results
  VVV = OSE_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$OSE   #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  
  OSE_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here ##geom_jitter( position = position_jitterdodge(jitter.width = .1, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(8,24))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression("OSE"), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  OSE_pic_final

  
  ######### 2b) SE - Self esteem   ###########################
  ## caution: had problematic distribution
  bigger =  mean(data$SE, na.rm = T)+2.5*sd(data$SE, na.rm = T)
  smaller =  mean(data$SE, na.rm = T)-2.5*sd(data$SE, na.rm =T)
  new_data = data[!(data$SE< smaller | data$SE> bigger),]
  
  SE_LMER0 <- lmer(SE ~  time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
  SE_LMER <- lmer(SE ~  time_ord*group +  (1|ID)+ school,data=new_data, na.action= na.omit)
  
  #check distributions of UV & AV  -> normal
  plot(check_distribution(SE_LMER))
  hist(residuals(SE_LMER))
  
  # permutation test
  Mperm_SE_sum_LMER = permlmer(SE_LMER0,SE_LMER,  seed = 123)
  Mperm_SE_sum_LMER2= permmodels(SE_LMER, type = 2, seed = 123)
  
  #summarize results
  SE_LMER_table = tab_model(SE_LMER, show.std = T)
  
  # get mean values and ci's
  #confint(SE_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(SE_LMER, list(pairwise ~ time_ord  ), adjust = "tukey")
  
  # plot the results
  VVV = SE_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$SE  #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  XX1 = na.omit(XX1)
  
  SE_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here ##geom_jitter( position = position_jitterdodge(jitter.width = .2, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(5,24))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression("SE"), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  SE_pic_final
  
######### 2d) GR - general resilience ###########################
  bigger =  mean(data$GR)+2.5*sd(data$GR)
  smaller =  mean(data$GR)-2.5*sd(data$GR)
  new_data = data[!(data$GR< smaller | data$GR> bigger),]
  
  GR_LMER0 <- lmer(GR ~ time_ord+group +  (1|ID)+ school,data=new_data, na.action= na.omit)
  GR_LMER <- lmer(GR ~ time_ord*group + (1|ID)+ school,data=new_data, na.action= na.omit)
  
  #check distributions of UV & AV  -> normal
  plot(check_distribution(GR_LMER))
  hist(residuals(GR_LMER))
  
  # permutation test
  Mperm_GR_sum_LMER = permlmer(GR_LMER0,GR_LMER,  seed = 123)
  Mperm_GR_sum_LMER2= permmodels(GR_LMER, type = 2, seed = 123)
  
  #summarize results
  GR_LMER_table= tab_model(GR_LMER, show.std = T, show.est= F)
  
  # get mean values and ci's
  #confint(GR_LMER, method = "boot") #-> bootstrapped CIs but dont work
  emmeans(GR_LMER, list(pairwise ~ time_ord | group ), adjust = "tukey")
  emmeans(GR_LMER, list(pairwise ~ time_ord  ), adjust = "tukey")
  
  
  # plot the results
  VVV = GR_LMER
  YYY = ggpredict(VVV , c( 'time_ord','group')) #%>%plot()+ ggplot2::theme_classic() +
  YYY$group = factor(YYY$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control"))
  YYY$fit = YYY$predicted
  YYY$Timepoint = YYY$x
  
  XX1 = as.data.frame(new_data$MOT_all_mean)# data.frame(rep(0, length(predict(VVV))))
  XX1$fit<- new_data$GR  #predict(VVV)
  XX1$group <- factor(new_data$group, ordered = F, levels = c( "I","C" ),labels = c("Intervention", "Control")) #factor(VVV@frame$group, ordered = F, levels = c( "I","C" ))
  XX1$Timepoint  <- new_data$time     #VVV@frame$time_ord 
  XX1$ID = new_data$ID
  XX1 = na.omit(XX1)
  
  GR_pic_final= 
    ggplot(XX1, aes(Timepoint, fit, color = group)) +
    geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
    geom_line(aes(group = ID), color = "gray", size = .2) +
    geom_point(position = position_jitterdodge(jitter.width = .3, dodge.width = .08), shape = 21, fill = "darkgray", size = .8) +  # Adjust point size here ##geom_jitter( position = position_jitterdodge(jitter.width = .2, dodge.width = .08) , shape = 21,fill =  "darkgray")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), data = YYY, width = .4, position = position_dodge(1.2), color = "black") +
    scale_fill_manual(values = c( "#FF8106","lightgray"))+ coord_cartesian(ylim=c(7,24))+
    theme_cowplot()+guides(fill = FALSE, colour = FALSE) + labs(y= expression("GR"), fill = "Group", x= "Timepoint",show.legend = FALSE)+ 
    facet_wrap(~group) + 
    scale_color_manual(values = c("darkgray", "darkgray"), guide = FALSE)+ 
    geom_point(data = YYY, aes(fill =group,), size = 4,shape = 20, color = "black") +
    geom_line(data = YYY, aes(group = group), color = "black", size = .8, linetype = 1) +
    theme(strip.background = element_blank())
  GR_pic_final
  
##################### 3) Plot everything together ###################
library(ggpubr)
  
fig1_MOT <- ggarrange(MOT_confidence_pic_final,
                      MOT_openness_pic_final, MOT_concern_pic_final,
                  MOT_responsibility_pic_final, 
                  common.legend = TRUE, legend = "none",
                  #labels = c("A", "B", "C", 'D'),
                  ncol = 2, nrow = 2)
fig1_MOT2  <- ggarrange(MOT_all_pic_final,fig1_MOT,
                        common.legend = TRUE, legend = "bottom",
                        #labels = c("A", "B", "C", 'D'),
                        ncol = 2, nrow = 1)#, 

fig2_RAT <- ggarrange(
                  SE_pic_final,
                 GR_pic_final,
                  OSE_pic_final, 
                  common.legend = TRUE, legend = "none",
                  #labels = c("A", "B", "C", 'D'),
                  ncol = 2, nrow = 2)
fig2_RAT2 <- ggarrange(CCSE_pic_final,fig2_RAT,
                      common.legend = TRUE, legend = "right",
                      #labels = c("A", "B", "C", 'D'),
                      ncol = 2, nrow = 1)#,

#summarize model estimates for MOTIVATION
tab_model(MOT_all_sum_LMER1, MOT_Zuversicht_LMER1,MOT_Offenheit_LMER, show.std = T,show.est = FALSE)#note: for p-values used permutation tests
tab_model(MOT_Eigenverantwortung_LMER,MOT_Betroffenheit_LMER, show.std = T,show.est = FALSE)#note: for p-values used permutation tests

#summarize model estimates for RATSCHINKSY SCALE
tab_model(CCSE_LMER,OSE_LMER, show.std = T ,show.est = FALSE) #note: for p-values used permutation tests
tab_model(SE_LMER,GR_LMER, show.std = T ,show.est = FALSE)#note: for p-values used permutation tests