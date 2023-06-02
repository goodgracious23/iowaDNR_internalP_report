setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Water Chemistry")

wq = read.csv("Carp_WQ_2018-2020.csv")
library(gridExtra)
library(hrbrthemes)
library(RColorBrewer)
brewer.pal(7,"Dark2")

center = wq %>% 
  filter(lake_id==19) %>% 
  # select(-year:-zoop_id, -lake_id, -lake_name,
  #        -doy:-vss_flag, -iss_flag:-phyco_flag) %>%
  unite(col = category, program:depth, sep = "_")
CEhypo<-"#A6761D" #brown for Center Lake
CEepi <- rgb(166, 118, 29, max = 255, alpha = 150)
CEalm <- rgb(166, 118, 29, max = 255, alpha = 70)


fiveisl = wq %>% 
  filter(lake_id==36) %>% 
  # select(-year:-zoop_id, -lake_id, -lake_name,
  #        -doy:-vss_flag, -iss_flag:-phyco_flag) %>%
  unite(col = category, program:depth, sep = "_")
FIhypo<-"#D95F02" #orange, Five Island
FIepi <- rgb(217, 95, 2, max = 255, alpha = 150)
FIalm <- rgb(217, 95, 2, max = 255, alpha = 70)

ntwin = wq %>% 
  filter(lake_id==90) %>% 
  # select(-year:-zoop_id, -lake_id, -lake_name,
  #        -doy:-vss_flag, -iss_flag:-phyco_flag) %>%
  unite(col = category, program:depth, sep = "_")
NThypo<-"#7570B3" #purpleblue, North Twin
NTepi <- rgb(117,112,179, max = 255, alpha = 150)
NTalm <- rgb(117,112,179, max = 255, alpha = 70)

silver = wq %>% 
  filter(lake_id==105) %>% 
  # select(-year:-zoop_id, -lake_id, -lake_name,
  #        -doy:-vss_flag, -iss_flag:-phyco_flag) %>%
  unite(col = category, program:depth, sep = "_")
SIhypo<-"#E7298A" #pink, Silver
SIepi <- rgb(231, 41, 138, max = 255, alpha = 150)
SIalm <- rgb(231, 41, 138, max = 255, alpha = 70)

storm = wq %>% 
  filter(lake_id==113) %>% 
  # select(-year:-zoop_id, -lake_id, -lake_name,
  #        -doy:-vss_flag, -iss_flag:-phyco_flag) %>%
  unite(col = category, program:depth, sep = "_")
SOhypo<-"#E6AB02" #mustard, Storm
SOepi <- rgb(230, 171, 2, max = 255, alpha = 150)
SOalm <- rgb(230, 171, 2, max = 255, alpha = 70)

stwin = wq %>% 
  filter(lake_id==406) %>% 
  # select(-year:-zoop_id, -lake_id, -lake_name,
  #        -doy:-vss_flag, -iss_flag:-phyco_flag) %>%
  unite(col = category, program:depth, sep = "_")
SThypo<-"#1B9E77" #teal, South Twin
STepi <- rgb(27, 158, 119, max = 255, alpha = 150)
STalm <- rgb(27, 158, 119, max = 255, alpha = 70)



CEiss <- ggplot(data = center, aes(x = category, y = iss_mgL)) +
  geom_boxplot(fill = c(CEalm, CEepi, CEhypo), outlier.size = 0) +
  geom_jitter(color="black", size = 2, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") + coord_cartesian(ylim = c(0, 35))

FIiss <- ggplot(data = fiveisl, aes(x = category, y = iss_mgL)) +
  geom_boxplot(fill = c(FIalm, FIepi, FIhypo), outlier.size = 0) +
  geom_jitter(color="black", size = 2, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") + coord_cartesian(ylim = c(0, 35))

NTiss <- ggplot(data = ntwin, aes(x = category, y = iss_mgL)) +
  geom_boxplot(fill = c(NTalm, NTepi, NThypo), outlier.size = 0) +
  geom_jitter(color="black", size = 2, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") + coord_cartesian(ylim = c(0, 35))

SIiss <- ggplot(data = silver, aes(x = category, y = iss_mgL)) +
  geom_boxplot(fill = c(SIalm, SIepi, SIhypo), outlier.size = 0) +
  geom_jitter(color="black", size = 2, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") + coord_cartesian(ylim = c(0, 35))

SOiss <- ggplot(data = storm, aes(x = category, y = iss_mgL)) +
  geom_boxplot(fill = c(SOalm, SOepi, SOhypo), outlier.size = 0) +
  geom_jitter(color="black", size = 2, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") + coord_cartesian(ylim = c(0, 35))

STiss <- ggplot(data = stwin, aes(x = category, y = iss_mgL)) +
  geom_boxplot(fill = c(STalm, STepi, SThypo), outlier.size = 0) +
  geom_jitter(color="black", size = 2, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("")

grid.arrange(CEiss, FIiss, NTiss, SIiss, SOiss, STiss, 
             nrow = 2)
