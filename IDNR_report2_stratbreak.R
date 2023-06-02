# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 2: Management and Restoration Recommendations
# Last modified: 1 December 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Exploratory analysis of stratification frequency data, break point analysis

# Data Citations:
#   1. [IDNR, ALM] Iowa Department of Natural Resources. 2022. 21IOWA: Iowa DNR Surface Water Monitoring Data (data and metadata files). Available from https://programs.iowadnr.gov/aquia/Facilities/6/Detail Iowa DNR AQuIA Database. Date accessed: 2022-10-05.

# The code can be run with the following data sets (in order first used in script)
#   1. "ALM Data 2000-2021 WIDE.csv" - water chemistry results from Ambient Lake Monitoring Program (IDNR, ALM) 2000-2021


library(tidyverse)

alm = read.csv("ALM Data 2000-2021 WIDE.csv") 
static = read.csv("ALM Lakes Static Characteristics Table.csv")

#Thermocline 
tcline = alm %>% 
  filter(year>2007) %>% 
  select(siteID, thermoclineDepth, year, doy) %>%
  mutate(thermoclineDepth = case_when(is.na(.$thermoclineDepth) ~ 0, 
                              TRUE ~ thermoclineDepth))

tcline_sum = tcline %>%
 count(thermoclineDepth>1, siteID) %>%
  rename(tcline_logical = `thermoclineDepth > 1`) %>%
  pivot_wider(id_cols = c(siteID), 
              names_from = tcline_logical, 
              values_from = n,
              values_fill = 0) %>%
  rename(no_tcline = `FALSE`,
         yes_tcline = `TRUE`) %>%
  mutate(n_tcline_obs = no_tcline + yes_tcline,
         tcline_percent = (yes_tcline/n_tcline_obs)*100) %>%
  filter(n_tcline_obs>9) 

windows(height = 4.5, width = 6)
plot(rank(tcline_sum$tcline_percent), tcline_sum$tcline_percent)
hist(tcline_sum$tcline_percent, ylab = "Number of Lakes", xlab = "% of observations Thermocline Observed (2007-2020)", cex.lab = 1, main = "")

#pair with lake depth, surface area
tcline_static = left_join(tcline_sum, static, by = "siteID")

plot(tcline_static$tcline_percent, tcline_static$meanDepth_m,
     ylim = c(12,0), pch = 19, col = rgb(102,102,102, max = 255, alpha = 150), cex = 2, xlab = "Observations with Thermocline", ylab = "Mean Depth (m)")

ggplot(data=tcline_static, aes(y=meanDepth_m, x=tcline_percent))+
  geom_point(alpha=0.5)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_abline(intercept=1.712519, slope=0.018023)


# create a linear model of obs thermocline ~ mean depth
my.lm<-lm(meanDepth_m ~tcline_percent, data = tcline_static)
summary(my.lm)

install.packages("segmented")
library(segmented)

my.break<-segmented(my.lm, 
                    seg.Z = ~ tcline_percent, 
                    psi = list(tcline_percent = c(80))) #starting value for the 
summary(my.break)
my.break$psi #get the break point: estimate=74.8, st. error=8.2
slope(my.break) #slope 1=0.00709, slope2=0.062246
intercept(my.break) #intercept1=1.9859, intercept2=-2.1395


ggplot(data=tcline_static, aes(y=meanDepth_m, x=tcline_percent))+
  geom_rect(aes(xmin=(74.8-8.2), xmax=(74.8+8.2), ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_point(alpha=0.5)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_abline(intercept=1.9859, slope=0.00709, color="red")+
  geom_abline(intercept=-2.1395, slope=0.062246, color="red")+
  geom_vline(xintercept=74.8)


# Now trying maximum depth
ggplot(data=tcline_static, aes(y=maxDepth_m, x=tcline_percent))+
  geom_point(alpha=0.5)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

# create a linear model of obs thermocline ~ max depth
my.lm.max<-lm(maxDepth_m ~tcline_percent, data = tcline_static)
summary(my.lm.max)
my.breakmax<-segmented(my.lm.max, 
                    seg.Z = ~ tcline_percent, 
                    psi = list(tcline_percent = c(75)))
summary(my.breakmax)
my.breakmax$psi #get the break point: estimate=79.8, st. error=4.6
slope(my.breakmax) #slope 1=0.027387, slope2=0.27177
intercept(my.breakmax) #intercept1=4.4792, intercept2=-15.0280

windows(height = 3.5, width = 4.5)
ggplot(data=tcline_static, aes(y=maxDepth_m, x=tcline_percent))+
  geom_abline(intercept=-4.4792, slope=-0.027387, color="white")+
  geom_abline(intercept=15.0280, slope=-0.27177, color="white")+
  geom_rect(aes(xmin=(79.8-4.6), xmax=(79.8+4.6), ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_segment(aes(x=79.8, y=6.6592, xend=100, yend=12.149), size=1, color="cadetblue")+
  geom_segment(aes(x=0, y=4.4792, xend=79.8, yend=6.66446), size=1, color="cadetblue")+
  geom_point(color="cadetblue",alpha=0.8, size=3)+ylim(25,0)+
  theme_linedraw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.y = element_text(color="black",size=9),
        axis.text.x = element_text(color="black",size=11),legend.position="none",axis.title=element_text(size=11))+
  xlab("% Observations with Thermocline")+ylab("Maximum Depth (m)")+
  geom_vline(xintercept=79.8, size=1)
ggsave("thermoclinebreak.png", width=4.5, height=3.5, units="in", dpi=300)
  

4.4792+0.027387*79.8 #6.664 m
-15.0280+0.27177*79.8 #6.659 m

4.4792+0.027387*(79.8-4.6) #6.54m
-15.0280+0.27177*(79.8+4.6) #7.9 m

citation("segmented")


library(grid)


tcline_contour = tcline_static %>%
  select(tcline_percent, maxDepth_m, surfaceArea_km2) %>%
  filter(!is.na(maxDepth_m),
         !is.na(surfaceArea_km2))


plot1 <- ggplot(tcline_contour, aes(x = maxDepth_m, y = surfaceArea_km2, z = tcline_percent)) +
  stat_contour()
plot1
