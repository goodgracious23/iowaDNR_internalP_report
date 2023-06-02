# Libraries
if (!require(tidyverse)) install.packages('tidyverse')
library('tidyverse')
if (!require(lubridate)) install.packages('lubridate')
library('lubridate')

# ALM Profile Data from 2019===========================
#Round 1 =================
hypoR1 = read.csv("ALM_2019Round1_Profiles.csv") %>%
  rename(name = Lake,
         btm_depth = Depth..m.,
         btm_pH = pH,
         btm_turb = Turbidity..NTU.,
         btm_do_sat = DO....Saturation.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(btm_depth == max(btm_depth)) %>%
  mutate(date = parse_date_time(Date, orders = "mdy"),
         date = as.POSIXct(date),
         doy = yday(date)) %>%
  select(name, doy, btm_depth, btm_pH, btm_do_sat, btm_temp, btm_turb)

# Round 2 ===================================
hypoR2 = read.csv("ALM_2019Round2_Profiles.csv") %>%
  rename(name = Lake,
         btm_depth = Depth..m.,
         btm_pH = pH,
         btm_turb = Turbidity..NTU.,
         btm_do_sat = DO....Saturation.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(btm_depth == max(btm_depth)) %>%
  mutate(date = parse_date_time(Date, orders = "mdy"),
         date = as.POSIXct(date),
         doy = yday(date)) %>%
  select(name, doy, btm_depth, btm_pH, btm_do_sat, btm_temp, btm_turb)


# Round 3 ===================================
hypoR3 = read.csv("ALM_2019Round3_Profiles.csv") %>%
  rename(name = Lake,
         btm_depth = Depth..m.,
         btm_pH = pH,
         btm_turb = Turbidity..NTU.,
         btm_do_sat = DO....Saturation.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(btm_depth == max(btm_depth)) %>%
  mutate(date = parse_date_time(DATE, orders = "mdy"),
         date = as.POSIXct(date),
         doy = yday(date)) %>%
  select(name, doy, btm_depth, btm_pH, btm_do_sat, btm_temp, btm_turb)

#Join everything together into one data frame
profile_join1 = left_join(hypoR1, hypoR2, by = "name", suffix = c("R1", "R2"))
profile_join = left_join(profile_join1, hypoR3, by = "name", suffix = c("", "R3")) %>%
  rename(btm_do_satR3 = btm_do_sat,
         btm_tempR3 = btm_temp,
         btm_pHR3 = btm_pH,
         btm_turbR3 = btm_turb,
         doyR3 = doy) %>%
  select(-btm_depthR1, -btm_depth, -btm_depthR2)

#GO LONG!!
hypo_long = profile_join %>%
  pivot_longer(cols = where(is.numeric),
               names_to = "variable",
               values_to = "values") %>%
  separate(variable, into = c("var", "id"), sep = -2) %>%
  pivot_wider(id_cols = c(name, id),
              names_from = var,
              values_from = values)

#Bring in the static characteristics table
static = read.csv("ALM Lakes Static Characteristics Table.csv") %>% rename(name = lakeName)
profile19 = left_join(hypo_long, static, by = c('name'))

###!!! THIS IS NOT CORRECT QUITE YET
alm19 = read.csv("ALM Data 2000-2021 WIDE.csv") %>% filter(year==2019)
alm_profiles19 = left_join(alm19, profile19, by = c("siteID", "doy")) %>%
  filter(!is.na(btm_do_sat)) 

if (!require(party)) install.packages('party')
library(party)
if (!require(partykit)) install.packages('partykit')
library(partykit)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(abind)) install.packages('abind')
library(abind)
if (!require(mlr)) install.packages('mlr')
library(mlr)
if (!require(pROC)) install.packages('pROC')
library(pROC)
if (!require(pdp)) install.packages('pdp')
library(pdp)


cforest19 = alm_profiles19 %>%
  select(TP, chlorophyll, TN_calc, SRP, latitude, longitude,
         epi_pH, btm_pH, thermoclineDepth, CaCO3, tss, iss,
         surfaceArea_km2, maxDepth_m, meanDepth_m, btm_do_sat) %>%
  mutate(vss = tss-iss)


cforest_btmDO<-partykit::cforest(btm_do_sat ~ 
                                   # TP + chlorophyll +  
                                   epi_pH + thermoclineDepth +
                                   surfaceArea_km2 + latitude +
                                   maxDepth_m,
                                   # CaCO3 + tss + iss + vss + 
                                  # meanDepth_m,
                                 data = cforest19, 
                                 na.action=na.pass, ntree=5000) #make the random forest


varimp_btmDO<-partykit::varimp(cforest_btmDO, conditional=F)
par(mar=c(8,4,1,1))
barplot(varimp_btmDO, las=2) 
preds<-predict(cforest_btmDO)

windows(height = 5, width = 6.5)
par(mfrow = c(2,3), mai = c(0.5,0.3,0.1,0.1), omi = c(0.5,0.5,0.1,0.1))
#Thermocline Partial Dependence
pd.tcline<-partial(cforest_btmDO, pred.var="thermoclineDepth", train = cforest19, which.class = 2, prob = T)
plot(pd.tcline, type = "l", ylab = "Bottom DO %Sat", xlab = "Thermocline Depth (m)", lwd = 3, col="dodgerblue3")
points(cforest19$thermoclineDepth, cforest19$btm_do_sat, pch = 20)

#Epilimnion pH Partial Dependence
pd.pH<-partial(cforest_btmDO, pred.var="epi_pH", train = cforest19, which.class=2, prob=T)
plot(pd.pH, type="l", ylab="Bottom DO %Sat", xlab = "pH", lwd=3, col="dodgerblue3")
points(cforest19$epi_pH, cforest19$btm_do_sat)

#Z max Partial Dependence
pd.zmax<-partial(cforest_btmDO, pred.var="maxDepth_m", train = cforest19, which.class = 2, prob = T)
plot(pd.zmax, type = "l", ylab = "Bottom DO %Sat", xlab = "Max Depth (m)", lwd = 3, col="dodgerblue3")
points(cforest19$maxDepth_m, cforest19$btm_do_sat)

#Surface Area Partial Dependence
pd.surfArea<-partial(cforest_btmDO, pred.var="surfaceArea_km2", train = cforest19, which.class = 2, prob = T)
plot(pd.surfArea, type = "l", ylab = "Bottom DO %Sat", xlab = "Surface Area (km2)", lwd = 3, col="dodgerblue3")
points(cforest19$surfaceArea_km2, cforest19$btm_do_sat)

#Latitude Partial Dependence
pd.latitude<-partial(cforest_btmDO, pred.var="latitude", train = cforest19, which.class = 2, prob = T)
plot(pd.latitude, type = "l", ylab = "Bottom DO %Sat", xlab = "Latitude", lwd = 3, col="dodgerblue3")
points(cforest19$latitude, cforest19$btm_do_sat)
