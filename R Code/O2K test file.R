##Run the Read Excel package, dplyr, ggplot2##
##IF NEEDED: Install package via install.package(readxl)##
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(zeallot)
library(reshape2)
library(Rmisc)
library(car)
library(compute.es)
library(multcomp)
library(pastecs)
library(mc2d)

##------------------------------------------------------------------------------------------------------------------------##
##FOR CARBOHYDRATE DATA##
#Import CHO data#
#file.name <- read_excel("fille_path.xlsx", sheet = "Sheet Name", col_names = TRUE)#

##------------------------------------------------------------------------------------------------------------------------##
#--------------------CHO t-test-----------------------#
CHO_ttest <- read_excel("FILE NAME GOES HERE", sheet = "Final Data t-test", col_names = TRUE)

##------------------------------------------------------------------------------------------------------------------------##
#-------------------CHO ANOVA-------------------------#
CHO_ANOVA <- read_excel("C:\\Users\\Stephen\\Desktop\\CHAMP_CHO_O2K_Analysis.xlsx", sheet = "Final Data ANOVA", col_names = TRUE)

#check structure of data frame#
str(CHO_ANOVA)

#remove excess column
#CHO_ANOVA$...3<- NULL

#remove rows with NA (select number of rows to include) and cytc#
CHO_ANOVA <- CHO_ANOVA[1:140,]
CHO_ANOVA<-CHO_ANOVA %>% dplyr::filter(row_number() %% 10 != 7) ## Delete every 10th row starting from 7

#set variables as categorical#
#use file.name$column <- as.factor(file.name$column)
CHO_ANOVA$Subject<-as.factor(CHO_ANOVA$Subject)
CHO_ANOVA$Condition<-as.factor(CHO_ANOVA$Condition)
CHO_ANOVA$Leg<-as.factor(CHO_ANOVA$Leg)
CHO_ANOVA$`Respiration State`<- factor(CHO_ANOVA$`Respiration State`, levels = c("Basal", "GM", "GMD(50)", "GMD(250)", "GMD(5000)", "GMDS", "Rot", "Omy", "AmA"))
CHO_ANOVA$`Respiration Rate (pmol/sec/mg)` <- as.numeric(CHO_ANOVA$`Respiration Rate (pmol/sec/mg)`)

#Levene's Test for a condition#
leveneTest(CHO_ANOVA$`Respiration Rate (pmol/sec/mg)`, CHO_ANOVA$Leg, center = median)

#Levene's Test for interaction#
leveneTest(CHO_ANOVA$`Respiration Rate (pmol/sec/mg)`, interaction(CHO_ANOVA$`Respiration State`,CHO_ANOVA$Leg), 
           center = median)

ANOVA_model<- aov(CHO_ANOVA$`Respiration Rate (pmol/sec/mg)` ~ CHO_ANOVA$Leg*CHO_ANOVA$`Respiration State`, data = CHO_ANOVA)
Anova(ANOVA_model)

summary.lm(ANOVA_model)
#-----------------Graph ANOVA------------------------#
#original Graph#
ggplot(CHO_ANOVA, aes(x=CHO_ANOVA$`Respiration State`, y=CHO_ANOVA$`Respiration Rate (pmol/sec/mg)`, 
                          fill = CHO_ANOVA$Leg)) +
  #Adds standard error bars#
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width=0.9), width = 0.3, size = 1) +
  #Adds Average bars#
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", colour = "black", size = 1) +
  #Graph labels#
  labs(x = "", y = "Respiration Rate (pmol/sec/mg)", fill = "Leg") +
  #Adds Individual data points#
  geom_jitter(aes(x=CHO_ANOVA$`Respiration State`, y=CHO_ANOVA$`Respiration Rate (pmol/sec/mg)`, 
                  fill = CHO_ANOVA$Leg, pch = Subject, colour = Subject),position = position_dodge(width=0.9)) +
  #Removes Data point from legend#
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #removes gridlines and top/left borders#
  theme_classic() +
  #Make y-axis start at 0, set limits#
  scale_y_continuous(expand = c(0, 0), limits = c(0,80), breaks=seq(0,80,10)) + 
  #Make bars different colors#
  scale_fill_manual(values = c("white", "black")) + 
  #Adjust fonts#
  theme(axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), 
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.text.y = element_text(color = "black", size = 10,),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 15, b = 0, l = 0)))
  
##------------------------------------------------------------------------------------------------------------------------##

##FOR FREE FATTY ACID DATA##
#Import FFA data#
#file.name <- read_excel("fille_path.xlsx", sheet = "Sheet Name")#

##------------------------------------------------------------------------------------------------------------------------##
#----------------------FFA t-test--------------------------#
FFA_ttest <- read_excel("FILE NAME GOES HERE", sheet = "Final Data t-test", col_names = TRUE)

##------------------------------------------------------------------------------------------------------------------------##
#-----------------------FFA ANOVA--------------------------#
FFA_ANOVA <- read_excel("FILE NAME GOES HERE", sheet = "Final Data ANOVA", col_names = TRUE)

#check structure of data frame#
str(FFA_ANOVA)

#remove excess column
#FFA_ANOVA$...3<- NULL

#remove rows with NA (select number of rows to include) and cytc#
FFA_ANOVA <- FFA_ANOVA[1:140,]
FFA_ANOVA<-FFA_ANOVA %>% dplyr::filter(row_number() %% 10 != 7) ## Delete every 10th row starting from 7

#set variables as categorical#
#use file.name$column <- as.factor(file.name$column)
FFA_ANOVA$Subject<-as.factor(FFA_ANOVA$Subject)
FFA_ANOVA$Condition<-as.factor(FFA_ANOVA$Condition)
FFA_ANOVA$Leg<-as.factor(FFA_ANOVA$Leg)
FFA_ANOVA$`Respiration State`<- factor(FFA_ANOVA$`Respiration State`, levels = c("Basal", "OctM", "OctMD(25)", "OctMD(50)", "OctMD(250)", "OctMD(5000)", "OctMDGS", "FCCP Peak", "Omy", "AmA"))
FFA_ANOVA$`Respiration Rate (pmol/sec/mg)` <- as.numeric(FFA_ANOVA$`Respiration Rate (pmol/sec/mg)`)

##Graph ANOVA##
#original Graph#
ggplot(FFA_ANOVA, aes(x=FFA_ANOVA$`Respiration State`, y=FFA_ANOVA$`Respiration Rate (pmol/sec/mg)`, 
                      fill = FFA_ANOVA$Leg)) +
  #Adds standard error bars#
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width=0.9), width = 0.3, size = 1) +
  #Adds Average bars#
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", colour = "black", size = 1) +
  #Graph labels#
  labs(x = "", y = "Respiration Rate (pmol/sec/mg)", fill = "Leg") +
  #Adds Individual data points#
  geom_jitter(aes(x=FFA_ANOVA$`Respiration State`, y=FFA_ANOVA$`Respiration Rate (pmol/sec/mg)`, 
                  fill = FFA_ANOVA$Leg, pch = Subject, colour = Subject),position = position_dodge(width=0.9)) +
  #Removes Data point from legend#
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #removes gridlines and top/left borders#
  theme_classic() +
  #Make y-axis start at 0, set limits#
  scale_y_continuous(expand = c(0, 0), limits = c(0,80), breaks=seq(0,80,10)) + 
  #Make bars different colors#
  scale_fill_manual(values = c("white", "black")) + 
  #Adjust fonts#
  theme(axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), 
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.text.y = element_text(color = "black", size = 10,),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 15, b = 0, l = 0)))
