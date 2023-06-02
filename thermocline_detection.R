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

windows(height = 3, width = 6)
par(mfrow = c(1,2), omi = c(0.5,0.1,0.1,0.1), mai = c(0.1, 0.7, 0.1, 0.1))
# plot(rank(tcline_sum$tcline_percent), tcline_sum$tcline_percent)
hist(tcline_sum$tcline_percent, 
     col = "darkgray", border = "white",
     ylab = "Number of Lakes", xlab = "% of observations Thermocline Observed (2007-2020)", 
     cex.lab = 1, main = "")

#pair with lake depth, surface area
tcline_static = left_join(tcline_sum, static, by = "siteID")

#Mean Depth Data and Model
plot(tcline_static$tcline_percent, tcline_static$meanDepth_m,
     ylim = c(0,16), pch = 19, 
     col = rgb(117,112,179, max = 255, alpha = 100), 
     cex = 1, xlab = "Observations with Thermocline", ylab = "Mean Depth (m)")

mean_model <- lm(log(meanDepth_m)~ tcline_percent, data = tcline_static)
s <- seq(1:100)
lines(s, (2.71828^mean_model$coefficients[1])*(2.71828^mean_model$coefficients[2])^s,
      lwd = 4, col = rgb(117,112,179, max = 255, alpha = 250))

#Max Depth Data and Model
points(tcline_static$tcline_percent, tcline_static$maxDepth_m,
       pch = 19, cex = 1, 
       col = rgb(27, 158, 119, max = 255, alpha = 100))

max_model <- lm(log(tcline_static$maxDepth_m)~ tcline_static$tcline_percent)
s <- seq(1:100)
lines(s, (2.71828^max_model$coefficients[1])*(2.71828^max_model$coefficients[2])^s,
      lwd = 4, col = rgb(27, 158, 119, max = 255, alpha = 250))
