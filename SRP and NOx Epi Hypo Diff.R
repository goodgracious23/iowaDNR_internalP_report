library(tidyverse)
library(gridExtra)
library(hrbrthemes)
library(RColorBrewer)
brewer.pal(7,"Dark2")
CEpre<-"#A6761D" #brown for Center Lake
FIpre<-"#D95F02" #orange, Five Island
NTpre <-"#7570B3" #purpleblue, North Twin
SIpre <-"#E7298A" #pink, Silver
SOpre <-"#E6AB02" #mustard, Storm
STpre <-"#1B9E77" #teal, South Twin
GVpre <-"#969696"

wq = read.csv("1_Carp_WQ_2018-2020.csv") %>%
  filter(program=="Carp") %>%
  filter(!lake_name=="Blue") %>%
  select(-sample_id:-program, -lake_id, 
         -year_frac:-tp_flag, -srp_flag:-tn_flag, 
         -nox_flag:-phyco_flag)

widerWQ = pivot_wider(wq, id_cols = c(year, lake_name, depth, doy),
                     names_from = depth,
                     values_from = c(srp_ugL, nox_mgL)) %>%
  mutate(srpDiff = srp_ugL_Hypo - srp_ugL_Epi,
         noxDiff = nox_mgL_Hypo - nox_mgL_Epi)

gvl = read.csv("GVL_srp_nox_diff.csv")

wideWQ = rbind(widerWQ, gvl)
wideWQ$lake_name.f <- factor(wideWQ$lake_name, levels = c("Center", "Five Island", "North Twin", "Silver", "South Twin", "Storm", "Green Valley"))


# Boxplot of Differences
srpDiff_box = 
  ggplot(data = wideWQ, aes(x = lake_name.f, y = log10(srpDiff))) +
  geom_boxplot(fill = c(CEpre, FIpre, NTpre, SIpre, STpre, SOpre, GVpre), 
               outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 3, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", 
        plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
  xlab("") + ylab("")

noxDiff_box = 
  ggplot(data = wideWQ, aes(x = lake_name.f, y = noxDiff)) +
  geom_boxplot(fill = c(CEpre, FIpre, NTpre, SIpre, STpre, SOpre, GVpre), 
               outlier.size = 0) +
  geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
              size = 3, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none", 
        plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
  xlab("") + ylab("") + coord_cartesian(ylim = c(-0.5, 0.5))

diff_box <- grid.arrange(srpDiff_box, noxDiff_box, nrow = 1)
