library(tidyverse)
# ALM Profile Data from 2018===========================
#Round 1 =================
surf_almR1 = read.csv("ALM2018R1_Profiles_Final_v3.csv") %>%
  rename(surf_depth = Depth..m.,
         surf_do_sat = DO....Saturation.,
         surf_do_mgL = DO..mg.L.,
         surf_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(surf_depth == 0) %>%
  select(name, surf_depth, surf_do_sat, surf_do_mgL, surf_temp)

hypo_almR1 = read.csv("ALM2018R1_Profiles_Final_v3.csv") %>%
  rename(btm_depth = Depth..m.,
         btm_do_sat = DO....Saturation.,
         btm_do_mgL = DO..mg.L.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(btm_depth == max(btm_depth)) %>%
  select(name, btm_depth, btm_do_sat, btm_do_mgL, btm_temp)

R1 = left_join(surf_almR1, hypo_almR1, by = c("name")) %>%
  mutate(diff_do_sat = surf_do_sat - btm_do_sat,
         diff_do_mgL = surf_do_mgL - btm_do_mgL,
         diff_temp = surf_temp - btm_temp) %>%
  select(-surf_depth, -btm_depth, -surf_do_sat:-surf_temp, -btm_temp)

# Round 2 ===================================
surf_almR2 = read.csv("ALM2018R2_Profiles_Final.csv") %>%
  rename(surf_depth = Depth..m.,
         surf_do_sat = DO....Saturation.,
         surf_do_mgL = DO..mg.L.,
         surf_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(surf_depth == 0) %>%
  select(name, surf_depth, surf_do_sat, surf_do_mgL, surf_temp)


hypo_almR2 = read.csv("ALM2018R2_Profiles_Final.csv") %>%
  rename(btm_depth = Depth..m.,
         btm_do_sat = DO....Saturation.,
         btm_do_mgL = DO..mg.L.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(btm_depth == max(btm_depth)) %>%
  select(name, btm_depth, btm_do_sat, btm_do_mgL, btm_temp)

R2 = left_join(surf_almR2, hypo_almR2, by = c("name")) %>%
  mutate(diff_do_sat = surf_do_sat - btm_do_sat,
         diff_do_mgL = surf_do_mgL - btm_do_mgL,
         diff_temp = surf_temp - btm_temp) %>%
  select(-surf_depth, -btm_depth, -surf_do_sat:-surf_temp, -btm_temp)


# Round 3 ===================================
surf_almR3 = read.csv("ALM2018R3_Profiles_Final.csv") %>%
  rename(surf_depth = Depth..m.,
         surf_do_sat = DO....Saturation.,
         surf_do_mgL = DO..mg.L.,
         surf_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(surf_depth == 0) %>%
  select(name, surf_depth, surf_do_sat, surf_do_mgL, surf_temp)

hypo_almR3 = read.csv("ALM2018R3_Profiles_Final.csv") %>%
  rename(btm_depth = Depth..m.,
         btm_do_sat = DO....Saturation.,
         btm_do_mgL = DO..mg.L.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(name) %>%
  filter(btm_depth == max(btm_depth)) %>%
  select(name, btm_depth, btm_do_sat, btm_do_mgL, btm_temp)

R3 = left_join(surf_almR3, hypo_almR3, by = c("name")) %>%
  mutate(diff_do_sat = surf_do_sat - btm_do_sat,
         diff_do_mgL = surf_do_mgL - btm_do_mgL,
         diff_temp = surf_temp - btm_temp) %>%
  select(-surf_depth, -btm_depth, -surf_do_sat:-surf_temp, -btm_temp)

#Join everything together into one data frame
profile_join1 = left_join(R1, R2, by = "name", suffix = c("R1", "R2"))
profile_join = left_join(profile_join1, R3, by = "name", suffix = c("", "R3")) %>%
  rename(btm_do_satR3 = btm_do_sat,
         btm_do_mgLR3 = btm_do_mgL,
         diff_do_satR3 = diff_do_sat,
         diff_do_mgLR3 = diff_do_mgL,
         diff_tempR3 = diff_temp) %>%
  mutate(mean_diff_temp = 
           mean(c(diff_tempR1, diff_tempR2, diff_tempR3), na.rm = TRUE),
         mean_btm_do_sat = 
           mean(c(btm_do_satR1, btm_do_satR2, btm_do_satR3), na.rm = TRUE),
         mean_diff_do_sat = 
           mean(c(diff_do_satR1, diff_do_satR2, diff_do_satR3), na.rm = TRUE)) %>%
  select(-btm_do_satR1:-diff_tempR3)

#Bring in the static characteristics table
static = read.csv("ALM Lakes Static Characteristics Table.csv") %>% rename(name = lakeName)
profile18 = left_join(profile_join, static, by = c('name'))

alm18 = read.csv("ALM Data 2000-2021 WIDE.csv") %>% filter(year==2018)
alm_profiles18 = left_join(alm18, profile18, by = "siteID") %>%
  filter(!is.na(mean_btm_do_sat))
  
alm18_summary = alm18 %>%
  group_by(siteID) %>%
  summarize(
    meanChl = mean(chlorophyll, na.rm = TRUE),
    maxChl = max(chlorophyll, na.rm = TRUE),
    meanTP = mean(TP, na.rm = TRUE)) %>%
  ungroup()
alm18_summary = as.data.frame(alm18_summary)

plot(rank(alm18_summary$maxChl), alm18_summary$maxChl)
plot(rank(alm18_summary$maxChl) ~ log10(alm18_summary$meanTP))

x = as.vector(alm18_summary$maxChl)
y = as.vector(alm18_summary$meanTP*1000)

fit <- nls(y ~ SSlogis(x, Asym, xmid, scal))
summary(fit)

plot(y~x)
lines(seq(0,250, length.out = 100), 
      predict(fit, newdata = data.frame(x = seq(0, 250, length.out = 100))))
# 
# ggplot(data = alm_profiles18, aes(x = mean_btm_do_sat, y = TN_calc))+
#   geom_point( col = "cadetblue4", size = 2)+
#   theme_linedraw()+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#   xlab("Bottom DO %Sat")+ylab("")
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

cforest18 = alm_profiles18 %>%
  select(TP, chlorophyll, TN_calc, SRP, 
           epi_pH, thermoclineDepth, 
           surfaceArea_km2, maxDepth_m, meanDepth_m, mean_btm_do_sat)


cforest_btmDO<-partykit::cforest(mean_btm_do_sat ~ 
                                   TP + chlorophyll + TN_calc + SRP + 
                                   epi_pH + thermoclineDepth +
                                   log10(surfaceArea_km2),
                                 data = cforest18, na.action=na.pass, ntree=1000) #make the random forest

par(mar=c(8,4,1,1))
varimp_btmDO<-partykit::varimp(cforest_btmDO, conditional=F)
barplot(varimp_btmDO, las=2) 
preds<-predict(cforest_btmDO)

windows(height = 2.5, width = 6.5)
par(mfrow = c(1,3), mai = c(0.3,0.3,0.1,0.1), omi = c(0.5,0.5,0.1,0.1))
#Thermocline Partial Dependence
# pd.tcline<-partial(cforest_btmDO, pred.var="thermoclineDepth", train = cforest18, which.class = 2, prob = T)
plot(pd.tcline, type = "l", ylab = "Bottom DO %Sat", xlab = "Thermocline Depth (m)", lwd = 3, col="dodgerblue3", ylim = c(0,120))
points(cforest18$thermoclineDepth, cforest18$mean_btm_do_sat)

#Epilimnion pH Partial Dependence
# pd.pH<-partial(cforest_btmDO, pred.var="epi_pH", train = cforest18, which.class=2, prob=T)
plot(pd.pH, type="l", ylab="Bottom DO %Sat", xlab = "pH", lwd=3, col="dodgerblue3", ylim = c(0,120))
points(cforest18$epi_pH, cforest18$mean_btm_do_sat)

#TP Partial Dependence
# pd.tp<-partial(cforest_btmDO, pred.var="TP", train = cforest18, which.class = 2, prob = T)
# plot(pd.tp, type = "l", ylab = "Bottom DO %Sat", xlab = "TP", lwd = 3, col="dodgerblue3", ylim = c(0,120))
# points(cforest18$TP, cforest18$mean_btm_do_sat)

#Surface Area Partial Dependence
# pd.surfArea<-partial(cforest_btmDO, pred.var="surfaceArea_km2", train = cforest18, which.class = 2, prob = T)
plot(pd.surfArea, type = "l", ylab = "Bottom DO %Sat", xlab = "Surface Area (km2)", lwd = 3, col="dodgerblue3", ylim = c(0,120))
points(cforest18$surfaceArea_km2, cforest18$mean_btm_do_sat)
