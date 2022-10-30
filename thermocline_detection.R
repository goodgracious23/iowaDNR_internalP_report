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
     ylim = c(12,0), pch = 19, col = rgb(102,102,102, max = 255, alpha = 150), cex = 3, xlab = "Observations with Thermocline", ylab = "Mean Depth (m)")

tcline_contour = tcline_static %>%
  select(tcline_percent, maxDepth_m, surfaceArea_km2) %>%
  filter(!is.na(maxDepth_m),
         !is.na(surfaceArea_km2))


plot1 <- ggplot(tcline_contour, aes(x = maxDepth_m, y = surfaceArea_km2, z = tcline_percent)) +
  stat_contour()
plot1
