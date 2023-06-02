# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 1: Phosphorus-Cycling Dynamics and Dominant Pathways in Iowa Lakes
# Last modified: 27 October 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Analysis code to evaluate change in macrophyte coverage, biomass, and richness following an incentivized carp harvest

# Data citations: 
#   2. [IDNR, Carp] Albright, E.A., G.M. Wilkinson, T.J. Butts, and Q.K. Shingai. 2022. Summer water chemistry; sediment phosphorus fluxes and sorption capacity; sedimentation and sediment resuspension dynamics; water column thermal structure; and zooplankton, macroinvertebrate, and macrophyte communities in eight shallow lakes in northwest Iowa, USA (2018-2020) ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/1d3797fd573208bae6f78963479445a0 (Accessed 2022-11-07).

# The code can be run with the following data sets (in order first used in script)
#   1. "9_Carp_Macrophytes_2018-20_Summary.csv" - summary of macrophyte survey data from seven lakes in north west Iowa ("carp lakes") before/during/after incentivized carp harvest (IDNR, Carp)
#   2. "8_Carp_Macrophytes_2018-20.csv" - macrophyte survey data from seven lakes in north west Iowa ("carp lakes") before/during/after incentivized carp harvest (IDNR, Carp)
#   3. "Mac_transect_detect.csv" - summary of the number of transects where macrophytes were detected as a proxy for coverage, hand-entered by E. Albright

# Packages needed to run the script (download as needed)
library(tidyverse)

setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Macrophytes")
# Read in macrophyte data for carp lakes (IDNR, Carp)
mac_summary<-read.csv("Carp_Macrophytes_2018-20_Summary.csv")
mac<-read.csv("Carp_Macrophytes_2018-20.csv")
mac_detect<-read.csv("Mac_transect_detect.csv")
mac_detect$year.f<-as.factor(mac_detect$year) # make year a factor

library(gridExtra)
library(hrbrthemes)

detectMac <- ggplot(data = mac_detect, aes(x = lake_name, y = per_transects)) +
  geom_point() +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") + coord_cartesian(ylim = c(0, 100))

# Part 1: Macrophyte coverage --------------------------------------------------------------------------------------------------------------------
# Figure: summarize macrophyte coverage (as % transects with macrophytes) across lakes, color-code by reference lake or pre vs. post carp harvest
transect_detect<-
ggplot(data = mac_detect) +
  geom_bar(stat = "identity", aes(x = year.f, 
                                  y = per_transects, 
                                  color = exp_condition, 
                                  fill = exp_condition)) +
  # scale_color_manual(values = c("#35978f","#8c510a","grey20")) +
  # scale_fill_manual(values = c("#c7eae5","#bf812d","grey80")) +
  facet_wrap(~lake_name, nrow = 3) +
  xlab(' ') + ylab("% Transects with Macrophytes") + ylim(0,100) +
  theme(axis.text = element_text(color = "black", size = 9),
        legend.position = "none",axis.title = element_text(size = 11)) +
  theme_linedraw() + theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank())
# ggsave("macrophyte_detect.png", transect_detect, width=8, height=7, units="in", dpi=300)

# Part 2: Macrophyte biomass and richness 
# summarize mean rake total (qualitative estimate of biomass), species richness, and submersed species richness by transect 
mac_sum <- mac %>%
  group_by(lake_name, year, transect) %>%
  summarise(across(c(rake_total, species_richness, submersed_richness), 
                   ~mean(., na.rm=T))) %>% 
  ungroup()
# manually designate the experimental condition for each lake and year (reference, pre-harvest, post-harvest)
mac_sum$year.f <- as.factor(mac_sum$year)
mac_sum$exp_condition[mac_sum$lake_name=="Storm"] <- "Reference"
mac_sum$exp_condition[mac_sum$lake_name=="South Twin"] <- "Reference"
mac_sum$exp_condition[mac_sum$lake_name=="Center" & mac_sum$year==2018] <- "Pre-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Center" & mac_sum$year==2019] <- "Post-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Center" & mac_sum$year==2020] <- "Post-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Five Island" & mac_sum$year==2018] <- "Pre-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Five Island" & mac_sum$year==2019] <- "Post-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Five Island" & mac_sum$year==2020] <- "Post-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="North Twin" & mac_sum$year==2018] <- "Pre-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="North Twin" & mac_sum$year==2019] <- "Pre-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="North Twin" & mac_sum$year==2020] <- "Post-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Silver" & mac_sum$year==2018] <- "Pre-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Silver" & mac_sum$year==2019] <- "Pre-Harvest"
mac_sum$exp_condition[mac_sum$lake_name=="Silver" & mac_sum$year==2020] <- "Post-Harvest"

# Figure: summarize qualitative macrophyte biomass (rake total) across lakes, color-code by reference lake or pre vs. post carp harvest
mac_raketotal<-
ggplot(data=mac_sum)+
  geom_boxplot(aes(x=year.f, y=rake_total, color=exp_condition, fill=exp_condition))+
  scale_color_manual(values=c("#35978f","#8c510a","grey20"))+
  scale_fill_manual(values=c("#c7eae5","#bf812d","grey80"))+
  facet_wrap(~lake_name, nrow=3)+
  xlab(' ') + ylab("Rake Total")+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggsave("macrophyte_raketotal.png", mac_raketotal, width=8, height=7, units="in", dpi=300)

# Figure: summarize macrophyte species richness across lakes, color-code by reference lake or pre vs. post carp harvest
ggplot(data=mac_sum)+
  geom_boxplot(aes(x=year.f, y=species_richness, color=exp_condition, fill=exp_condition))+
  scale_color_manual(values=c("#35978f","#8c510a","grey20"))+
  scale_fill_manual(values=c("#c7eae5","#bf812d","grey80"))+
  facet_wrap(~lake_name, nrow=3)+
  xlab(' ') + ylab("Species Richness")+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

# Figure: summarize macrophyte submersed species richness across lakes, color-code by reference lake or pre vs. post carp harvest
submersed_rich<-
ggplot(data=mac_sum)+
  geom_boxplot(aes(x=year.f, y=submersed_richness, color=exp_condition, fill=exp_condition))+
  scale_color_manual(values=c("#35978f","#8c510a","grey20"))+
  scale_fill_manual(values=c("#c7eae5","#bf812d","grey80"))+
  facet_wrap(~lake_name, nrow=3)+
  xlab(' ') + ylab("Submersed Species Richness")+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggsave("macrophyte_subrich.png", submersed_rich, width=8, height=7, units="in", dpi=300)
