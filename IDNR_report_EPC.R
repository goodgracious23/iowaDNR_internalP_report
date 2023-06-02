# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 1: Phosphorus-Cycling Dynamics and Dominant Pathways in Iowa Lakes
# Last modified: 28 October 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Analysis code to summarize equilibrium phosphorus concentrations (EPC) for select Iowa lakes and reservoirs

# Data citations: 
#   2. [IDNR, Carp] Albright, E.A., G.M. Wilkinson, T.J. Butts, and Q.K. Shingai. 2022. Summer water chemistry; sediment phosphorus fluxes and sorption capacity; sedimentation and sediment resuspension dynamics; water column thermal structure; and zooplankton, macroinvertebrate, and macrophyte communities in eight shallow lakes in northwest Iowa, USA (2018-2020) ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/1d3797fd573208bae6f78963479445a0 (Accessed 2022-11-07).
#   3. [GVL] Albright, E.A., and G.M. Wilkinson. 2021. Spatiotemporal variation in internal phosphorus loading, sediment characteristics, water column chemistry, and thermal mixing in a hypereutrophic reservoir in southwest Iowa, USA (2019-2020) ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/d3a70c1f0d534cca8bdebd7f7483ef38 (Accessed 2021-10-14).

# The code can be run with the following data sets (in order first used in script)
#   1. "14_Carp_EPC_2018_CALC.csv" - summary of EPC values for seven lakes in north west Iowa, the "carp lakes" (IDNR, Carp)
#   2. "EPC_2019_GVL.csv" - summary of EPC values at six sites in Green Valley Lake ("GVL") reservoir in southwest Iowa (GVL)
#   3. "1_Carp_WQ_2018-2020.csv" - water chemistry data for seven lakes in north west Iowa (IDNR, Carp)

# Packages needed to run the script (download as needed)
library(tidyverse)
library(gridExtra)

# Set up working directory - update to fit your system
getwd()
# setwd('C:/Users/Ellen/Desktop/Box Sync/POSTDOC/IDNR/New_Analyses')

# Read in EPC data for the carp lakes and GVL ---------------------------------------------------------------------------
# Carp EPC Data
epc_carp<-read.csv("14_Carp_EPC_2018_CALC.csv")
# GVL EPC Data
epc_gvl<-read.csv("EPC_2019_GVL.csv")

# FIGURE ----------------------------------------------------------------------------------------------------------------
# Panel 1: Carp Lakes EPC -----------------------------------------------------------------------------------------------
epc_carp<-epc_carp %>% 
  filter(EPC_flag!="f") # filter out failed assays 
epc_carp$lake_name<-factor(epc_carp$lake_name, levels=c("Swan","Center","Storm","Five Island","Silver")) # order lakes by EPC

carp_epc_fig<-
ggplot(data=epc_carp, aes(x=lake_name, y=EPC_ugL)) +
  geom_bar(stat = "identity", color = "black", fill = "#08519c")+
  ylim(0,250)+
  xlab(' ') + 
  ylab(bquote('Equilibrium Phosphorus Concentration (µg ' *L^-1*')'))+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

# Panel 2: GVL EPC -----------------------------------------------------------------------------------------------
epc_gvl$site_id<-factor(epc_gvl$site_id, levels=c("1","2","3","4","5","6")) # order sites from west to east

gvl_epc_fig<-
ggplot(data = epc_gvl, aes(x = site_id, y = EPC_ugL)) +
  geom_bar(stat = "identity",color = "black",aes(fill = site_id))+
  ylim(0,250) +
  scale_fill_manual(values=c("#c6dbef", "#9ecae1", "#3182bd", "#08519c", "#6baed6", "#eff3ff"))+
  xlab('Sampling Site') +  ylab("") +
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position="none")

windows(height = 4, width = 6.5)
epc_figure<-grid.arrange(carp_epc_fig, gvl_epc_fig, nrow=1)
# ggsave("IDNR_epc.png", epc_figure, width=9, height=7, units="in", dpi=300)



# Further Analysis:
# Based on mean bottom water SRP concentrations and EPC - are re-suspended sediments likely a source of P to the water column?
# read in water chemistry data for carp lakes, summarize bottom water SRP data
wq<-read.csv("1_Carp_WQ_2018-2020.csv")
wq_sum_hypo<-wq %>% 
  filter(depth=="Hypo") %>% 
  group_by(lake_name) %>% 
  mutate(mean_srp=mean(srp_ugL)) %>% 
  slice(n=1) %>% 
  ungroup()
# bottom water SRP is usually less than EPC for these lakes, so re-suspended sediments are usually a source of P to the water column

