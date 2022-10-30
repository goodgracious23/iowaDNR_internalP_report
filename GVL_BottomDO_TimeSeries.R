# Green Valley Bottom DO
library(tidyverse)
#======== SITE COLORS =========#
site1 = "#045275"
site2 = "#089099"
site3 = "#7CCBA2"
site4 = "#FCDE92"
site5 = "#F0746E"
site6 = "#CF597E"

gvl_btm = read.csv("field_profiles_gvl_2019.csv") %>%
  group_by(doy, siteID) %>%
  filter(depth == max(depth)) %>%
  select(doy, siteID, depth, do_sat, temperature) %>%
  ungroup()
gvl_btm = as.data.frame(gvl_btm)

windows(height = 5, width = 5)
plot(gvl_btm[gvl_btm$siteID==1, "doy"],
     gvl_btm[gvl_btm$siteID==1, "do_sat"],
     pch = 19, cex = 1, lwd = 2, col = site1, type = "o",
     ylim = c(0,200), xlab = "Day of Year", ylab = "Bottom DO % Saturation")
polygon(c(140,250,250,140), c(0,0,20,20), border = FALSE, 
        col = rgb(102,102,102, max = 255, alpha = 25))
points(gvl_btm[gvl_btm$siteID==2, "doy"],
     gvl_btm[gvl_btm$siteID==2, "do_sat"],
     pch = 19, cex = 1, lwd = 2, col = site2, type = "o")
points(gvl_btm[gvl_btm$siteID==3, "doy"],
       gvl_btm[gvl_btm$siteID==3, "do_sat"],
       pch = 19, cex = 1, lwd = 2, col = site3, type = "o")
points(gvl_btm[gvl_btm$siteID==4, "doy"],
       gvl_btm[gvl_btm$siteID==4, "do_sat"],
       pch = 19, cex = 1, lwd = 2, col = site4, type = "o")
points(gvl_btm[gvl_btm$siteID==5, "doy"],
       gvl_btm[gvl_btm$siteID==5, "do_sat"],
       pch = 19, cex = 1, lwd = 2, col = site5, type = "o")
points(gvl_btm[gvl_btm$siteID==6, "doy"],
       gvl_btm[gvl_btm$siteID==6, "do_sat"],
       pch = 19, cex = 1, lwd = 2, col = site6, type = "o")
