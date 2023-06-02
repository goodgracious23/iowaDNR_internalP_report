library(tidyverse)
library(lubridate)

tmdl = read.csv("TMDL_SuspendedSolids.csv") %>%
  filter(type=="Lake") %>% #filter to lake only data
  mutate(date = parse_date_time(sampleDate, orders = "mdy HM"), #make sense of the dates
         date = as.POSIXct(date),
         doy = yday(date), #create a day of year column
         year = year(date)) %>% #create a year column
  select(-facilityID, -county:-projectCode, -analyte:-relDepth, -unit:-remark, -date) %>% #get rid of the riff raff
pivot_wider(id_cols = c(year, doy, siteID, name),
            names_from = cas_rn,
            values_from = result,
            values_fn = {mean})

multiDepths = read.csv("TMDL_MultipleDepths.csv") %>% 
  select(-TFSS, -name, -siteID) %>%
  pivot_wider(id_cols = c(year, doy, lake), 
              names_from = site, 
              values_from = TSS) %>%
  group_by(lake, year) %>%
  summarize(meanSurface = mean(`surface water`, na.rm = TRUE),
  sdSurface = sd(`surface water`, na.rm = TRUE),
  meanMid = mean(`mid depth`, na.rm = TRUE),
  sdMid = sd(`mid depth`, na.rm = TRUE),
  meanBottom = mean(`bottom water`, na.rm = TRUE),
  sdBottom = sd(`bottom water`, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(meanSurface))

plot(multiDepths$meanSurface, ylim = c(0,60), pch = 19, cex = 1.5, col = "dodgerblue1")
points(multiDepths$meanMid, pch = 19, cex = 1.5, col = "dodgerblue3")
points(multiDepths$meanBottom, pch = 19, cex = 1.5, col = "dodgerblue4")

multiSites = read.csv("TMDL_MultipleSites.csv")
boxplot(multiSites$TSS ~ multiSites$siteID, las = 2, xlab = "")

hfTSS = read.csv("TMDL_HigherFrequencySampling.csv")
boxplot(hfTSS$TSS~hfTSS$lake)
boxplot()
