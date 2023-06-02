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

setwd("C:/Users/grace/Box/Carp Project Iowa DNR/IDNR Carp REPORT/iowaDNR_internalP_report")
pop = read.csv("IncentivizedHarvestRemoval_PopEstimates.csv") %>%
        mutate(surface_ha = area_acres * 0.404686,
               biomass_kgha = (biomass_lb * 0.453592)/surface_ha,
               bio_CIlow = (biomass_Cilow * 0.453592)/surface_ha,
               bio_CIhigh = (biomass_Cihigh * 0.453592)/surface_ha) %>%
        select(-area_acres:-biomass_Cihigh)

# write.csv(pop, file = "Population_Harvest_kgha.csv")

carp = pop %>% filter(species == 'carp')
bmb = pop %>% filter(species == 'bmb')


carp_plot <- ggplot(carp) +
        geom_bar( aes(x = lakeyear, y = biomass_kgha), 
                  stat = "identity", alpha = 0.7,
                  fill = c(CEpre, CEpre, CEpre, CEpre, "white",
                           FIpre, FIpre, FIpre, "white",
                           NTpre, NTpre, NTpre, NTpre, "white",
                           SIpre, SIpre, SIpre, "white",
                           STpre, STpre, STpre, STpre, "white",
                           SOpre, SOpre, SOpre)) +
        geom_errorbar( aes(x = lakeyear, 
                           ymin = bio_CIlow, 
                           ymax = bio_CIhigh), 
                       width = 0.4, alpha = 0.9, size = 1.3,
                       colour=c(CEpre, CEpre, CEpre, CEpre, "white",
                                FIpre, FIpre, FIpre, "white",
                                NTpre, NTpre, NTpre, NTpre, "white",
                                SIpre, SIpre, SIpre, "white",
                                STpre, STpre, STpre, STpre, "white",
                                SOpre, SOpre, SOpre)) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 300))



bmb_plot <- ggplot(bmb) +
        geom_bar( aes(x = lakeyear, y = biomass_kgha), 
                  stat = "identity", alpha = 0.7,
                  fill = c(CEpre, CEpre, CEpre, CEpre, "white",
                           FIpre, FIpre, FIpre, "white",
                           NTpre, NTpre, NTpre, NTpre, "white",
                           SIpre, SIpre, SIpre, "white",
                           STpre, STpre, STpre, STpre, "white",
                           SOpre, SOpre, SOpre)) +
        geom_errorbar( aes(x = lakeyear, 
                           ymin = bio_CIlow, 
                           ymax = bio_CIhigh), 
                       width = 0.4, alpha = 0.9, size = 1.3,
                       colour=c(CEpre, CEpre, CEpre, CEpre, "white",
                                FIpre, FIpre, FIpre, "white",
                                NTpre, NTpre, NTpre, NTpre, "white",
                                SIpre, SIpre, SIpre, "white",
                                STpre, STpre, STpre, STpre, "white",
                                SOpre, SOpre, SOpre)) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 600))

# windows(height = 6, width = 8)
populations <- grid.arrange(carp_plot, bmb_plot, nrow = 2)
