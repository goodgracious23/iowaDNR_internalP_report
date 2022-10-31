library(tidyverse)
library(viridis)
library(patchwork)

setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Sensors")
temp = read.csv("Carp_Temp_2018-19.csv")

# Center Lake - 2018 ==================================
center18 = temp %>%
  filter(lake_id == 19, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
center18 = as.data.frame(center18)

# Center Lake - 2019 ================================
center19 = temp %>%
  filter(lake_id == 19, year == 2019) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
center19 = as.data.frame(center19)

# Five Island - 2018 ==================================
fiveisl18 = temp %>%
  filter(lake_id == 36, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
fiveisl18 = as.data.frame(fiveisl18)

# Five Island - 2019 ==================================
fiveisl19 = temp %>%
  filter(lake_id == 36, year == 2019) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
fiveisl19 = as.data.frame(fiveisl19)

# South Twin- 2018 ==================================
stwin18 = temp %>%
  filter(lake_id == 406, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
stwin18 = as.data.frame(stwin18)

# South Twin - 2019 ==================================
stwin19 = temp %>%
  filter(lake_id == 406, year == 2019) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
stwin19 = as.data.frame(stwin19)

# Swan Lake - 2018 ==================================
swan18 = temp %>%
  filter(lake_id == 114, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
swan18 = as.data.frame(swan18)

# Silver Lake - 2018 ==================================
silver18 = temp %>%
  filter(lake_id == 105, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
silver18 = as.data.frame(silver18)

# North Twin - 2018 ==================================
ntwin18 = temp %>%
  filter(lake_id == 90, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))
ntwin18 = as.data.frame(ntwin18)

# Green Valley - 2019 ================================
gvl19 = read.csv("GVL_Site4_Tchain.csv") %>%
  group_by(doy) %>%
  summarize(surface = mean(Temp_0, na.rm = TRUE),
            bottom = mean(Temp_6, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff_temp = surface - bottom)
gvl19 = as.data.frame(gvl19)

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
STcol<- "#1B9E77" #teal, South Twin
STtrans <- rgb(27, 158, 119, max = 255, alpha = 150)
SWcol<-"#66A61E" #green, Swan
SWtrans <- rgb(102, 166, 30, max = 255, alpha = 150)
GVcol <- "#666666"
GVtrans <- rgb(102, 102, 102, max = 255, alpha = 150)


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
     xlab = "", ylab = "", cex.axis = 1.2, xaxt = "n", yaxt = "n")
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