# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 1: Phosphorus-Cycling Dynamics and Dominant Pathways in Iowa Lakes
# Last modified: 29 December 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Analysis code to plot sediment phosphorus flux rates across Swan (SW), North Twin (NT), and South Twin (ST) lakes

# Data citations: 
#   1. [IDNR, Carp] Albright, E.A., G.M. Wilkinson, T.J. Butts, and Q.K. Shingai. 2022. Summer water chemistry; sediment phosphorus fluxes and sorption capacity; sedimentation and sediment resuspension dynamics; water column thermal structure; and zooplankton, macroinvertebrate, and macrophyte communities in eight shallow lakes in northwest Iowa, USA (2018-2020) ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/1d3797fd573208bae6f78963479445a0 (Accessed 2022-11-07).

# The code can be run with the following data sets (in order first used in script)
#   1. "Incubation_2019_explanatory.csv" - summary of sediment core incubation experiments from 2019, Swan and North Twin lakes

# Packages needed to run the script (download as needed)
library(ggplot2)

# Set up working directory - update to fit your system
getwd()
setwd('C:/Users/Ellen/Desktop/Box Sync/POSTDOC/IDNR/New_Analyses')

# Read in core incubation "explanatory dataset" - includes average Total Prr for each treatment and sd, context variables = OM, BD, sediment P, water depth
incubation<-read.csv('Incubation_2019_explanatory.csv')

# Look at lakes individually - SWAN
SW<-subset(incubation,Lake=="Swan")
sd<-SW$PrrTP_sd/1.414 #using standard error for error bars

# visualize mean P retention or release rates for each site and treatment - each bar represents mean of two replicate cores (averaged 4 daily release rates per core), standard error for bars
ggplot(data=SW,aes(x=ID,y=PrrTP_mean,fill=Treatment))+
  geom_bar(stat="identity",width=0.5)+scale_fill_manual(values=c("#ffb81a", "#6ab8c5"))+
  geom_linerange(aes(x=ID,y=PrrTP_mean,ymin=PrrTP_mean-sd,ymax=PrrTP_mean+sd))+
  ylim(-10,20)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_hline(yintercept=0,linetype="dashed")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  labs(y="Total P Release Rate (mgP/m2/day")

# repeat for north twin lake
NT<-subset(incubation,Lake=="North Twin")
sd2<-NT$PrrTP_sd/1.73
ggplot(data=NT,aes(x=ID,y=PrrTP_mean,fill=Treatment))+
  geom_bar(stat="identity",width=0.5)+scale_fill_manual(values=c("#ffb81a", "#6ab8c5"))+
  geom_linerange(aes(x=ID,y=PrrTP_mean,ymin=PrrTP_mean-sd2,ymax=PrrTP_mean+sd2))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_hline(yintercept=0,linetype="dashed")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  labs(y="Total P Release Rate (mgP/m2/day")+ylim(-10,20)

# add south twin, too
ST<-subset(dat,Lake=="South Twin")
View(ST)
sd3<-ST$PrrTP_sd/1.73
ggplot(data=ST,aes(x=ID,y=PrrTP_mean,fill=Treatment))+
  geom_bar(stat="identity",width=0.5)+scale_fill_manual(values=c("#ffb81a", "#6ab8c5"))+
  geom_linerange(aes(x=ID,y=PrrTP_mean,ymin=PrrTP_mean-sd3,ymax=PrrTP_mean+sd3))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_hline(yintercept=0,linetype="dashed")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  labs(y="Total P Release Rate (mgP/m2/day")+ylim(-10,20)
