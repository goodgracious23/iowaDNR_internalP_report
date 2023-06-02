setwd("C:/Users/grace/Box/Carp Project Iowa DNR/IDNR Carp REPORT/iowaDNR_internalP_report")

library(tidyverse)
library(gridExtra)
library(hrbrthemes)
library(RColorBrewer)

isco = read.csv("ISCO TP - GVL2019.csv")
gvl = read.csv("nutrients_gvl_2019.csv")

site1 = gvl %>% 
  filter(siteID==1) %>% 
  filter(depthID==1) %>% 
  filter(!is.na(tp))
site7 = isco %>% 
  filter(site==7) %>% 
  filter(!is.na(tp_mgL))

site6 = gvl %>% 
  filter(siteID==6) %>% 
  filter(depthID==1) %>% 
  filter(!is.na(tp))
site8 = isco %>% 
  filter(site==8) %>% 
  filter(!is.na(tp_mgL))

westArm = 
  ggplot(site7, aes(x = doy, y = tp_mgL*1000)) + 
  geom_point(size = 3, colour = "gray30") +
  ylim (0,1000) + xlim (130,240) +
  labs( x = "",
        y = "Total P (ug/L)") +
  theme_bw() + theme(legend.position = "none", 
                     plot.margin = unit(c(0.2,0.2,0.2,0.2),"in"),
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=14,face="bold")) +
  geom_point(data = site1, 
             mapping = aes(x=doy, y = tp), size = 4, colour = "dodgerblue3")

eastArm = 
  ggplot(site8, aes(x = doy, y = tp_mgL*1000)) + 
  geom_point(size = 3, colour = "gray30") +
  ylim (0,1000) + xlim (130,240) +
  labs( x = "",
        y = "Total P (ug/L)") +
  theme_bw() + theme(legend.position = "none", 
                     plot.margin = unit(c(0.2,0.2,0.2,0.2),"in"),
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=14,face="bold")) +
  geom_point(data = site6, 
             mapping = aes(x=doy, y = tp), size = 4, colour = "dodgerblue3")


iscoCompare <- grid.arrange(westArm, eastArm, nrow = 2)
