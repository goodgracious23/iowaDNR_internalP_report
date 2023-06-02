# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 1: Phosphorus-Cycling Dynamics and Dominant Pathways in Iowa Lakes
# Last modified: 21 November 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Analysis code to visualize differences in sediment phosphorus composition among seven lakes in north west Iowa ("carp lakes")

# Data citations: 
#   2. Albright EA, Wilkinson GM, Fleck RA, Shingai Q. 2020. Analysis of Sediment Phosphorus Pools in Shallow Lakes. figshare. https://doi.org/10.6084/m9.figshare.13362971.v1 

# The code can be run with the following data sets (in order first used in script)
#   1. "sedimentPcomp.csv" - summary of sediment phosphorus composition from seven lakes in north west Iowa ("carp lakes") 

# Packages needed to run the script (download as needed)
library(ggplot2)

# Set up working directory - update to fit your system
getwd()
setwd('C:/Users/Ellen/Desktop/Box Sync/POSTDOC/IDNR/New_Analyses')


# read in summary data of sediment phosphorus composition
TPbar<-read.csv("sedimentPcomp.csv")

sedimentP_barplot<-
  ggplot(data = TPbar, aes(x = lake, y = concentration),color="white") +
  ylim(0,1500)+
  geom_col(aes(fill = p_form), color="white",width = 0.7)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab(NULL) + ylab(bquote('P Concentration (µg P ' *g^-1*' Dry Sediment)'))+
  theme(legend.position = c(0.2,0.85),legend.text = element_text(size=15),legend.title=element_blank(),
        axis.text=element_text(color="black",size=12),axis.title=element_text(size=14),axis.text.y=element_text(angle=90,hjust=0.5))+
  scale_fill_manual(values=c("#3288bd","grey80","#d53e4f","#51AAAE","#f46d43"),
                    labels = c("Aluminum-Bound", "Calcium-Bound","Loosely-Bound", "Organic","Redox-Sensitive"))+
  labs(fill="P Species")
ggsave("IDNR_CarpLake_Pcomp.png", sedimentP_barplot, width=6.5, height=5.5, units="in", dpi=300)
