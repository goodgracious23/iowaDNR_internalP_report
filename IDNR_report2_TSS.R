# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 2: Management and Restoration Recommendations
# Last modified: 19 December 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Exploratory analysis of suspended solids data, break point analysis

# Data Citations:
#   1. [IDNR, ALM] Iowa Department of Natural Resources. 2022. 21IOWA: Iowa DNR Surface Water Monitoring Data (data and metadata files). Available from https://programs.iowadnr.gov/aquia/Facilities/6/Detail Iowa DNR AQuIA Database. Date accessed: 2022-10-05.
#   2. [LTER-NTL] 

# The code can be run with the following data sets (in order first used in script)
#   1. "ALM Data 2000-2021 WIDE.csv" - water chemistry results from Ambient Lake Monitoring Program (IDNR, ALM) 2000-2021

# Packages needed to run the script (download as needed)
library(tidyverse)
library(gridExtra)

# Set up working directory - update to fit your system
getwd()
# setwd('C:/Users/Ellen/Desktop/Box Sync/POSTDOC/IDNR/New_Analyses')


# Read in ALM water chemistry data (IDNR, ALM)
alm<-read.csv("ALM Data 2000-2021 WIDE.csv")
alm$iss_per<-alm$iss/alm$tss*100
alm<-filter(alm, iss_per<=100)
alm$logtss<-log10(alm$tss)

ggplot(data=alm_means, aes(y=log10(iss), x=log10(maxDepth)))+
  geom_point()
ggplot(data=alm_means, aes(y=log10(tss), x=log10(maxDepth)))+
  geom_point()


ggplot(data=alm, aes(y=iss, x=maxDepth, group=doy))+
  geom_point(aes(color=doy))
ggplot(data=alm, aes(y=iss_per, x=maxDepth, group=doy))+
  geom_point(aes(color=doy))
ggplot(data=alm, aes(y=iss_per, x=tss, group=doy))+
  geom_point(aes(color=doy))

ggplot(data=alm, aes(x=tss))+
  geom_histogram(binwidth=10)

ggplot(data=alm, aes(x=log10(tss), y=secchiDepth))+
  geom_point()


alm_means<-alm %>% 
  group_by(siteID, year) %>% 
  summarise(across(c(maxDepth, iss, tss, iss_per, TP, SRP, secchiDepth), ~mean(., na.rm=T))) %>%
  ungroup()
ggplot(data=alm_means, aes(x=log10(iss), y=log10(secchiDepth)))+
  geom_point()+
  geom_hline(yintercept=0, color="red")+
  geom_vline(xintercept=1, color="red")

ggplot(data=alm_means, aes(x=logiss, y=secchiDepth))+
  geom_point()+
  geom_hline(yintercept=1, color="red")+
  geom_vline(xintercept=0.69, color="red")

ggplot(data=alm_means, aes(y=tss, x=maxDepth))+
  geom_point(color="cadetblue4", alpha=0.5)+xlim(0,5)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
quantile(alm_means$tss, probs=c(0.9, 0.95)) #32, 45
quantile(alm_means$tss)

ggplot(data=alm_means, aes(y=log10(iss), x=maxDepth))+
  geom_point(color="cadetblue4", alpha=0.5)+xlim(0,10)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
quantile(alm_means$iss, probs=c(0.9, 0.95)) #17, 24

ggplot(data=alm_means, aes(x=tss))+
  geom_histogram(color="cadetblue4", alpha=0.5, binwidth=10)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
quantile(alm_means$iss_per, probs=c(0.9,0.95)) #72, 78

alm_means$logiss<-log10(alm_means$iss)
alm_means<-filter(alm_means, logiss!=-Inf)
alm_means$logdepth<-log10(alm_means$maxDepth)

# ALM log10(iss) ~ log10(maxDepth) break point analysis
# create a linear model of
my.lm<-lm(logdepth ~logiss, data = alm_means)
summary(my.lm)

install.packages("segmented")
library(segmented)

my.break<-segmented(my.lm, 
                    seg.Z = ~ logiss, 
                    psi = list(logiss = c(0.48)))
summary(my.break)
my.break$psi #get the break point: estimate=0.69, st. error=0.048
slope(my.break) #slope 1=-0.15894, slope2=-0.45031
intercept(my.break) #intercept1=0.87758, intercept2=1.08120

# ISS break:
10^0.69 #4.9 mg/L
10^0.048 # standard error is 1.1 mg/L
# corresponding lake depth is
0.87758-(0.15894*(0.69)) 
10^0.7679114 #5.86 m
0.87758-(0.15894*(0.69-0.048)) 
10^0.7755405 #5.96 m
1.08120-(0.45031*(0.69+0.048))
10^0.7488712 #5.61 m

ggplot(data=alm_means, aes(x=logiss, y=logdepth))+
  geom_abline(intercept=-0.87758, slope=0.15894, color="white")+
  geom_abline(intercept=-1.0812, slope=0.45031, color="white")+
  geom_point(alpha=0.5, color="chocolate")+ylim(2,-0.5)+
  geom_rect(aes(xmin=(0.69-0.048), xmax=(0.69+0.048), ymin=-Inf, ymax=Inf), color=NA, fill="grey80", alpha=0.5)+
  geom_segment(aes(x=-0.5, y=0.95705, xend=0.69, yend=0.76791148), size=1, color="chocolate4")+
  geom_segment(aes(x=0.69, y=0.7704861, xend=2.5, yend=-0.044575), size=1, color="chocolate4")+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_vline(xintercept=0.69, size=1)+
  theme(axis.text.y = element_text(color="black",size=9),
        axis.text.x = element_text(color="black",size=11),
        legend.position="none",axis.title=element_text(size=11))+
  xlab("Log10(ISS, mg/L)")+ylab("Log10(Max Depth, m)")
ggsave("issbreak.png", width=4.5, height=3.5, units="in", dpi=300)



# 0.87758-(0.15894*0.69)
# 1.08120-(0.45031*2.5)
# 
# ggplot(data=alm_means, aes(x=logdepth, y=logiss))+
#   geom_abline(intercept=1.3244, slope=-0.88292, color="white")+
#   geom_abline(intercept=0.80359, slope=-0.20072, color="white")+
#   geom_point(alpha=0.5, color="chocolate")+
#   geom_rect(aes(xmin=(0.76-0.026), xmax=(0.76+0.026), ymin=-Inf, ymax=Inf), color=NA, fill="grey80", alpha=0.5)+
#   geom_segment(aes(x=-0.5, y=1.769, xend=0.76, yend=0.648608), size=1, color="chocolate4")+
#   geom_segment(aes(x=0.76, y=0.648608, xend=1.5, yend=0.50251), size=1, color="chocolate4")+
#   theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#   geom_vline(xintercept=0.76, size=1)+
#   theme(axis.text.y = element_text(color="black",size=9),
#         axis.text.x = element_text(color="black",size=11),legend.position="none",axis.title=element_text(size=11))+
#   xlab("Log10(Maximum Depth, m)")+ylab("Log10(ISS, mg/L)")
# ggsave("issbreak.png", width=8, height=5, units="in", dpi=300)
# 
# 
# 1.3244-(0.88292*0.76) #0.6533
# 10^0.6533808 #4.5 mg L ISS - get error with this threshold
# 1.3244-(0.88292*0.6711728) #0.7318081
# 0.80359-(0.20072*0.8331471) #0.6363607
# 10^0.7318081 #5.39
# 10^0.6363607 #4.33
# # apply to madison lakes
# 10^0.76 #5.75 m
# 10^0.026 #1.06
# # 4.69 m to 6.81
# 
# log10(4.69)
# log10(6.81)




















alm_sum<-alm_means %>% 
  group_by(siteID) %>% 
  mutate(zmax=mean(maxDepth)) %>% 
  slice(n=1)
alm_sum_name<-merge(alm_sum, alm_stat, by="siteID")



















# testing tss thresholds with LTER-NTL data (TPM only, same as TSS)
# read in chemistry data (TPM for northern lakes only)
ntl_n<-read.csv("NTL_TPM_North.csv")
ntl_n<-filter(ntl_n, tpm>=0)
plot(hist(ntl_n$tpm))
summary(ntl_n$tpm) #median=0.8, 75th percentile=1.37
ntl_n20<-filter(ntl_n, tpm>=20) #27 observations 0.3% obs
ntl_n10<-filter(ntl_n, tpm>=10) #104 obs, 1.2% obs

# now the southern lakes 
ntl_s<-read.csv("tpmmadison.csv")
ntl_s<-filter(ntl_s, tpm>=0)
plot(hist(ntl_s$tpm))
summary(ntl_s$tpm) #median=0.77, 75th percentile=2.244
ntl_s20<-filter(ntl_s, tpm>=20) #3 obs 0.4% obs
ntl_s10<-filter(ntl_s, tpm>=10) #7 obs 1% obs

