library(tidyverse)
library(gridExtra)
library(hrbrthemes)
library(RColorBrewer)
brewer.pal(7,"Dark2")

setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Macrophytes")
# Read in macrophyte data for carp lakes (IDNR, Carp)
mac_summary<-read.csv("Carp_Macrophytes_2018-20_Summary.csv")
mac<-read.csv("Carp_Macrophytes_2018-20.csv")
mac_detect<-read.csv("Mac_transect_detect.csv")
mac_detect$year.f<-as.factor(mac_detect$year) # make year a factor

#
center = mac_detect %>% filter(lake_name=="Center")
CEpre<-"#A6761D" #brown for Center Lake
CEpost <- rgb(166, 118, 29, max = 255, alpha = 70)

fiveisl = mac_detect %>% filter(lake_name=="Five Island")
FIpre<-"#D95F02" #orange, Five Island
FIpost <- rgb(217, 95, 2, max = 255, alpha = 70)

ntwin = mac_detect %>% filter(lake_name=="North Twin") 
NTpre <-"#7570B3" #purpleblue, North Twin
NTpost <- rgb(117,112,179, max = 255, alpha = 70)

silver = mac_detect %>% filter(lake_name=="Silver") 
SIpre <-"#E7298A" #pink, Silver
SIpost <- rgb(231, 41, 138, max = 255, alpha = 70)

storm = mac_detect %>% filter(lake_name=="Storm") 
SOpre <-"#E6AB02" #mustard, Storm
SOpost <- rgb(230, 171, 2, max = 255, alpha = 70)

stwin = mac_detect %>% filter(lake_name=="South Twin") 
STpre <-"#1B9E77" #teal, South Twin
STpost <- rgb(27, 158, 119, max = 255, alpha = 70)


CEdetect <- ggplot(data = center, aes(x = year.f, y = per_transects)) +
  geom_col(size = 3, fill = c(CEpost, CEpost, CEpre)) + 
  theme_ipsum() + theme(legend.position = "none",
                        plot.margin = unit(c(0.2,0,0,0.1),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 100))

FIdetect <- ggplot(data = fiveisl, aes(x = year.f, y = per_transects)) +
  geom_col(size = 3, fill = c(FIpost, FIpost, FIpre)) +
  theme_ipsum() + theme(legend.position = "none",
                        plot.margin = unit(c(0.2,0,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 100))

NTdetect <- ggplot(data = ntwin, aes(x = year.f, y = per_transects)) +
  geom_col(size = 3, fill = c(NTpre, NTpost, NTpost)) +
  theme_ipsum() + theme(legend.position = "none",
                        plot.margin = unit(c(0.2,0.1,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 100))

SIdetect <- ggplot(data = silver, aes(x = year.f, y = per_transects)) +
  geom_col(size = 3, fill = c(SIpre, SIpost, SIpost)) +
  theme_ipsum() + theme(legend.position = "none",
                        plot.margin = unit(c(0.2,0,0,0.1),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 100))

SOdetect <- ggplot(data = storm, aes(x = year.f, y = per_transects)) +
  geom_col(size = 3, fill = c(SOpre, SOpre, SOpre)) +
  theme_ipsum() + theme(legend.position = "none",
                        plot.margin = unit(c(0.2,0,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 100))

STdetect <- ggplot(data = stwin, aes(x = year.f, y = per_transects)) +
  geom_col(size = 3, fill = c(STpre, STpre, STpre)) +
  theme_ipsum() + theme(legend.position = "none", 
                        plot.margin = unit(c(0.2,0.1,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 100))

# windows(height = 4, width = 6.5)
transect_detect <- grid.arrange(CEdetect, FIdetect, NTdetect, SIdetect,
             SOdetect, STdetect, 
             nrow = 2)
# ggsave("macrophyte_detect_carpLakes.png", transect_detect,
# width = 6.5, height = 4, units="in", dpi = 300)

#Summary Macrophytes ==================================
mac_sum <- mac %>%
  group_by(lake_name, year, transect) %>%
  summarise(across(c(rake_total, species_richness, submersed_richness), 
                   ~mean(., na.rm=T))) %>% 
  ungroup()
mac_sum$year.f <- as.factor(mac_sum$year)

center = mac_sum %>% filter(lake_name=="Center")
fiveisl = mac_sum %>% filter(lake_name=="Five Island")
ntwin = mac_sum %>% filter(lake_name=="North Twin") 
silver = mac_sum %>% filter(lake_name=="Silver") 
storm = mac_sum %>% filter(lake_name=="Storm") 
stwin = mac_sum %>% filter(lake_name=="South Twin") 

CEsum <- ggplot(data = center, aes(x = year.f, y = rake_total)) +
  geom_boxplot(fill = c(CEpre, CEpre, CEpost), outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 1.5, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", plot.margin = unit(c(0.2,0,0,0.1),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5))

FIsum <- ggplot(data = fiveisl, aes(x = year.f, y = rake_total)) +
  geom_boxplot(fill = c(FIpre, FIpre, CEpost), outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 1.5, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", plot.margin = unit(c(0.2,0,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5))

NTsum <- ggplot(data = ntwin, aes(x = year.f, y = rake_total)) +
  geom_boxplot(fill = c(NTpre, NTpost, NTpost), outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 1.5, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", plot.margin = unit(c(0.2,0.1,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5))

SIsum <- ggplot(data = silver, aes(x = year.f, y = rake_total)) +
  geom_boxplot(fill = c(SIpre, SIpost, SIpost), outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 1.5, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", plot.margin = unit(c(0.2,0,0,0.1),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5))

SOsum <- ggplot(data = storm, aes(x = year.f, y = rake_total)) +
  geom_boxplot(fill = c(SOpre, SOpre, SOpre), outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 1.5, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", plot.margin = unit(c(0.2,0,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5))

STsum <- ggplot(data = stwin, aes(x = year.f, y = rake_total)) +
  geom_boxplot(fill = c(STpre, STpre, STpre), outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 1.5, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", plot.margin = unit(c(0.2,0.1,0,0),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5))

# windows(height = 4, width = 6.5)
transect_sum <- grid.arrange(CEsum, FIsum, NTsum, 
                             SIsum,SOsum, STsum, nrow = 2)
ggsave("macrophyte_summary_carpLakes.png", transect_sum, 
       width = 6.5, height = 4, units="in", dpi = 300)

