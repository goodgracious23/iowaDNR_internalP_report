library(tidyverse)
library(viridis)
library(patchwork)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)
if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Sensors")
temp = read.csv("Carp_Temp_2018-19.csv")

# Colors
brewer.pal(7,"Dark2")
CEcol<-"#A6761D" #brown for Center Lake
CEtrans <- rgb(166, 118, 29, max = 255, alpha = 150)
FIcol<-"#D95F02" #orange, Five Island
FItrans <- rgb(217, 95, 2, max = 255, alpha = 150)
NTcol<-"#7570B3" #purpleblue, North Twin
NTtrans <- rgb(117,112,179, max = 255, alpha = 150)
SIcol<-"#E7298A" #pink, Silver
SItrans <- rgb(231, 41, 138, max = 255, alpha = 150)
SOcol<-"#E6AB02" #mustard, Storm
SOtrans <- rgb(230, 171, 2, max = 255, alpha = 150)
STcol<- "#1B9E77" #teal, South Twin
STtrans <- rgb(27, 158, 119, max = 255, alpha = 150)
SWcol<-"#66A61E" #green, Swan
SWtrans <- rgb(102, 166, 30, max = 255, alpha = 150)
GVcol <- "#666666"
GVtrans <- rgb(102, 102, 102, max = 255, alpha = 150)

# Center Lake - 2018 ==================================
center18 = temp %>%
  filter(lake_id == 19, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE)) %>%
  pivot_wider(id_cols = doy, names_from = depth_m, values_from = dtemp) %>%
  mutate(center18_diff = `0`-`3.5`) %>%
  select(-`0`:-`3.5`)

# Five Island - 2018 ==================================
fiveisl18 = temp %>%
  filter(lake_id == 36, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE)) %>%
  pivot_wider(id_cols = doy, names_from = depth_m, values_from = dtemp) %>%
  mutate(fiveisl18_diff = `0`-`6`) %>%
  select(-`0`:-`0.5`)

# South Twin- 2018 ==================================
stwin18 = temp %>%
  filter(lake_id == 406, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE)) %>%  
  pivot_wider(id_cols = doy, names_from = depth_m, values_from = dtemp) %>%
  mutate(stwin18_diff = `0.5`-`1.5`) %>%
  select(-`0.5`:-`1.5`)

# Swan Lake - 2018 ==================================
swan18 = temp %>%
  filter(lake_id == 114, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE)) %>%
  pivot_wider(id_cols = doy, names_from = depth_m, values_from = dtemp) %>%
  mutate(swan18_diff = `0`-`3`) %>%
  select(-`0`:-`3`)

# Silver Lake - 2018 ==================================
silver18 = temp %>%
  filter(lake_id == 105, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE)) %>%
  pivot_wider(id_cols = doy, names_from = depth_m, values_from = dtemp) %>%
  mutate(silver18_diff = `0.5`-`2.5`) %>%
  select(-`0.5`:-`2.5`)

# North Twin - 2018 ==================================
ntwin18 = temp %>%
  filter(lake_id == 90, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE)) %>%
  pivot_wider(id_cols = doy, names_from = depth_m, values_from = dtemp) %>%
  mutate(ntwin18_diff = `1`-`3.5`) %>%
  select(-`0.5`:-`3.5`)

tempDiff1 = left_join(center18, fiveisl18, by = "doy")
tempDiff2 = left_join(tempDiff1, stwin18, by = "doy")
tempDiff3 = left_join(tempDiff2, swan18, by = "doy")
tempDiff4 = left_join(tempDiff3, ntwin18, by = "doy")
tempDiff18 = left_join(tempDiff4, silver18, by = "doy") %>%
  pivot_longer(center18_diff:silver18_diff, names_to = "lakeYear")
tempDiff18$lakeYear <- factor(tempDiff18$lakeYear, 
                              levels = c("silver18_diff",
                                         "ntwin18_diff",
                                         "stwin18_diff",
                                         "swan18_diff",
                                         "fiveisl18_diff",
                                         "center18_diff"))

ggplot(tempDiff18, aes(doy, lakeYear, height = value, fill = lakeYear, group = lakeYear)) + 
  geom_density_ridges2(stat = "identity", 
                       scale = 4, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(SItrans, NTtrans, STtrans, SWtrans, SOtrans, CEtrans)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(135,255)) +
  labs(title = 'Temperature Difference')


plot(fiveisl18[fiveisl18$depth_m==0, "doy"],
     fiveisl18[fiveisl18$depth_m==0, "dtemp"] - 
       fiveisl18[fiveisl18$depth_m==6, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = "white", 
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n")
polygon(c(124, swan18[swan18$depth_m==0, "doy"], 254),
        c(0, swan18[swan18$depth_m==0, "dtemp"] - 
            swan18[swan18$depth_m==3, "dtemp"], 0), 
        col = SWtrans, border = NA)
polygon(c(135, fiveisl18[fiveisl18$depth_m==0, "doy"], 255),
        c(0, fiveisl18[fiveisl18$depth_m==0, "dtemp"] - 
            fiveisl18[fiveisl18$depth_m==6, "dtemp"], 0), 
        col = FItrans, border = NA)
polygon(c(135, center18[center18$depth_m==0, "doy"], 255),
        c(0, center18[center18$depth_m==0, "dtemp"] - 
            center18[center18$depth_m==3.5, "dtemp"], 0), 
        col = CEtrans, border = NA)
polygon(c(130, stwin18[stwin18$depth_m==0.5, "doy"], 255),
        c(0, stwin18[stwin18$depth_m==0.5, "dtemp"] - 
            stwin18[stwin18$depth_m==1.5, "dtemp"], 0), 
        col = STtrans, border = NA)
polygon(c(130, ntwin18[ntwin18$depth_m==1, "doy"], 158),
        c(0, ntwin18[ntwin18$depth_m==1, "dtemp"] - 
            ntwin18[ntwin18$depth_m==3.5, "dtemp"], 0), 
        col = NTtrans, border = NA)
polygon(c(136, silver18[silver18$depth_m==0.5, "doy"], 151),
        c(0, silver18[silver18$depth_m==0.5, "dtemp"] - 
            silver18[silver18$depth_m==2.5, "dtemp"], 0), 
        col = SItrans, border = NA)

windows(height = 9, width = 6.5)
par(mfrow = c(6,2), omi = c(0.5,0.5,0.1,0.1), mai = c(0.1,0.1,0,0.1))
#Five Island - 2018
plot(fiveisl18[fiveisl18$depth_m==0, "doy"],
     fiveisl18[fiveisl18$depth_m==0, "dtemp"] - 
       fiveisl18[fiveisl18$depth_m==6, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = FIcol, 
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n")
polygon(c(135, fiveisl18[fiveisl18$depth_m==0, "doy"], 255),
        c(0, fiveisl18[fiveisl18$depth_m==0, "dtemp"] - 
            fiveisl18[fiveisl18$depth_m==6, "dtemp"], 0), 
        col = FItrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.2, "Five Island")

#Five Island - 2019
plot(fiveisl19[fiveisl19$depth_m==0, "doy"],
     fiveisl19[fiveisl19$depth_m==0, "dtemp"] - 
       fiveisl19[fiveisl19$depth_m==6, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,265), col = FIcol, 
     xlab = "", ylab = "", cex.axis = 1.2)
polygon(c(135, fiveisl19[fiveisl19$depth_m==0, "doy"], 218),
        c(0, fiveisl19[fiveisl19$depth_m==0, "dtemp"] - 
            fiveisl19[fiveisl19$depth_m==6, "dtemp"], 0), 
        col = FItrans)
abline(1,0, lwd = 1, lty = 3)
text(235, 10, cex = 1.2, "Five Island")

#Center Lake - 2018
plot(center18[center18$depth_m==0, "doy"],
      center18[center18$depth_m==0, "dtemp"] - 
        center18[center18$depth_m==3.5, "dtemp"], 
      type = "l", lwd = 2, col = CEcol, 
     ylim = c(0,12), xlim = c(124,255), 
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n")
polygon(c(135, center18[center18$depth_m==0, "doy"], 255),
        c(0, center18[center18$depth_m==0, "dtemp"] - 
            center18[center18$depth_m==3.5, "dtemp"], 0), 
        col = CEtrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.2, "Center Lake")

#Center Lake - 2019
plot(center19[center19$depth_m==0, "doy"],
     center19[center19$depth_m==0, "dtemp"] - 
       center19[center19$depth_m==3.5, "dtemp"], 
     type = "l", lwd = 2, col = CEcol, 
     ylim = c(0,12), xlim = c(124,265), 
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n", yaxt = "n")
polygon(c(135, center19[center19$depth_m==0, "doy"], 170),
        c(0, center19[center19$depth_m==0, "dtemp"] - 
            center19[center19$depth_m==3.5, "dtemp"], 0), 
        col = CEtrans)
abline(1,0, lwd = 1, lty = 3)
text(235, 10, cex = 1.2, "Center Lake")

# South Twin - 2018
plot(stwin18[stwin18$depth_m==0.5, "doy"],
      stwin18[stwin18$depth_m==0.5, "dtemp"] - 
        stwin18[stwin18$depth_m==1.5, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = STcol,
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n")
polygon(c(130, stwin18[stwin18$depth_m==0.5, "doy"], 255),
        c(0, stwin18[stwin18$depth_m==0.5, "dtemp"] - 
            stwin18[stwin18$depth_m==1.5, "dtemp"], 0), 
        col = STtrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.2, "South Twin")

# South Twin - 2019
plot(stwin19[stwin19$depth_m==0.5, "doy"],
     stwin19[stwin19$depth_m==0.5, "dtemp"] - 
       stwin19[stwin19$depth_m==1.5, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,265), col = STcol,
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n", yaxt = "n")
polygon(c(130, stwin19[stwin19$depth_m==0.5, "doy"], 255),
        c(0, stwin19[stwin19$depth_m==0.5, "dtemp"] - 
            stwin19[stwin19$depth_m==1.5, "dtemp"], 0), 
        col = STtrans)
abline(1,0, lwd = 1, lty = 3)
text(235, 10, cex = 1.2, "South Twin")

#Swan Lake - 2018
plot(swan18[swan18$depth_m==0, "doy"],
     swan18[swan18$depth_m==0, "dtemp"] - 
       swan18[swan18$depth_m==3, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = SWcol,
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n")
polygon(c(124, swan18[swan18$depth_m==0, "doy"], 254),
        c(0, swan18[swan18$depth_m==0, "dtemp"] - 
            swan18[swan18$depth_m==3, "dtemp"], 0), 
        col = SWtrans)
abline(1,0, lwd = 1, lty = 3)
mtext(side = 2, line = 3, "                          Temperature Difference, Surface - Bottom", font = 2)
text(230, 10, cex = 1.2, "Swan Lake")

#Green Valley Lake - 2019
plot(gvl19$doy, gvl19$diff_temp, type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,265), col = GVcol,
     xlab = "", ylab = "", cex.axis = 1.2, yaxt = "n")
polygon(c(134, gvl19$doy, 263),
        c(0, gvl19$diff_temp, 0), 
        col = GVtrans)
abline(1,0, lwd = 1, lty = 3)
text(235, 10, cex = 1.2, "Green Valley")
mtext(side = 1, line = 3, "Day of Year, 2019", font = 2)


#North Twin Lake - 2018
plot(ntwin18[ntwin18$depth_m==1, "doy"],
     ntwin18[ntwin18$depth_m==1, "dtemp"] - 
       ntwin18[ntwin18$depth_m==3.5, "dtemp"], 
     type = "l", lwd = 2, col = NTcol, 
     ylim = c(0,12), xlim = c(124,255), 
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n")
polygon(c(130, ntwin18[ntwin18$depth_m==1, "doy"], 158),
        c(0, ntwin18[ntwin18$depth_m==1, "dtemp"] - 
            ntwin18[ntwin18$depth_m==3.5, "dtemp"], 0), 
        col = NTtrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.2, "North Twin")

#Blank
plot.new()

#Silver Lake - 2018
plot(silver18[silver18$depth_m==0.5, "doy"],
     silver18[silver18$depth_m==0.5, "dtemp"] - 
       silver18[silver18$depth_m==2.5, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = SIcol,
     xlab = "", ylab = "", cex.axis = 1.2)
polygon(c(136, silver18[silver18$depth_m==0.5, "doy"], 151),
        c(0, silver18[silver18$depth_m==0.5, "dtemp"] - 
            silver18[silver18$depth_m==2.5, "dtemp"], 0), 
        col = SItrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.2, "Silver Lake")
mtext(side = 1, line = 3, "Day of Year, 2018", font = 2)

#Blank
plot.new()



# High Frequency vs. ALM Comparison
plot(center18[center18$depth_m==0, "doy"],
     center18[center18$depth_m==0, "dtemp"] - 
       center18[center18$depth_m==3.5, "dtemp"], 
     type = "l", lwd = 2, col = CEcol, 
     ylim = c(0,10), xlim = c(134,255), 
     xlab = "", ylab = "", cex.axis = 1.2)
polygon(c(135, center18[center18$depth_m==0, "doy"], 255),
        c(0, center18[center18$depth_m==0, "dtemp"] - 
            center18[center18$depth_m==3.5, "dtemp"], 0), 
        col = CEtrans)
abline(1,0, lwd = 2, lty = 3)
# 7 stratified periods
library(hrbrthemes)

boxplot(stratified, mixed, pch = 1, lwd = 2, 
        col = c("#E7298A", "#1B9E77"), las = 2, cex.axis = 1.5, cex = 2)

center_periods = data.frame(regime= c(rep("stratified", 7), rep("mixed", 7)),
                            period = c(27,2,5,11,4,8,1,12,2,7,9,6,9,18)) %>%
  ggplot( aes(x = regime, y = period, fill = regime)) +
  geom_boxplot(fill = c(STtrans, SItrans), outlier.size = 0) +
  geom_jitter(color="black", size = 4, alpha = 0.5, width = 0.1) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("")

center_periods
