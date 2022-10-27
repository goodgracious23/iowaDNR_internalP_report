library(tidyverse)

alm = read.csv("ALM Data 2000-2021 WIDE.csv") 

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

