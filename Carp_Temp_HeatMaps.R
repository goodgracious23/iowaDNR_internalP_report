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

windows(height = 4, width = 6)
ggplot(center18, aes(doy, depth_m, fill = dtemp)) + 
  geom_tile() + xlim(135,255) +
  scale_fill_viridis(option="D", name="Temp (C)") +
  scale_y_reverse() +
  theme_linedraw() + theme(panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank())+
  xlab("Day of Year") + ylab("Depth (m)") + 
  theme(axis.text=element_text(color="black",size=10),
        legend.position = "none", 
        axis.title = element_text(size=12))

center18 = as.data.frame(center18)
plot(center18[center18$depth_m==0, "doy"],
     center18[center18$depth_m==0, "dtemp"], type = "l", lwd = 2,
     ylim = c(12,32), xlim = c(135,255), col = "gray60")
lines(center18[center18$depth_m==3.5, "doy"],
     center18[center18$depth_m==3.5, "dtemp"], 
     type = "l", lwd = 2, lty = 2)
lines(c(135,160), c(30,30), lwd = 6, col = rgb(38,130,142, max = 255))
lines(c(178,181), c(30,30), lwd = 6, col = rgb(38,130,142, max = 255))
lines(c(190, 200), c(30,30), lwd = 6, col = rgb(38,130,142, max = 255))
lines(c(209,215), c(30,30), lwd = 6, col = rgb(38,130,142, max = 255))
lines(c(220,228), c(30,30), lwd = 6, col = rgb(38,130,142, max = 255))

# Center Lake - 2019 ================================
center19 = temp %>%
  filter(lake_id == 19, year == 2019) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))

center19_heat <- ggplot(center19, aes(doy, depth_m, fill= dtemp)) + 
  geom_tile()+xlim(135,255)+
  scale_fill_viridis(option="D",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=10),legend.position="none",axis.title=element_text(size=12))


# Five Island - 2018 ==================================
fiveisl18 = temp %>%
  filter(lake_id == 36, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))

fiveisl18_heat <- ggplot(fiveisl18, aes(doy, depth_m, fill= dtemp)) + 
  geom_tile()+xlim(135,255)+
  scale_fill_viridis(option="D",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=10),legend.position="none",axis.title=element_text(size=12))

# Five Island - 2019 ==================================
fiveisl19 = temp %>%
  filter(lake_id == 36, year == 2019) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))

fiveisl19_heat <- ggplot(fiveisl19, aes(doy, depth_m, fill= dtemp)) + 
  geom_tile()+xlim(135,255)+
  scale_fill_viridis(option="D",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=10),legend.position="none",axis.title=element_text(size=12))


# South Twin- 2018 ==================================
stwin18 = temp %>%
  filter(lake_id == 406, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))

stwin18_heat <- ggplot(stwin18, aes(doy, depth_m, fill= dtemp)) + 
  geom_tile()+xlim(135,255)+
  scale_fill_viridis(option="D",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=10),legend.position="none",axis.title=element_text(size=12))


# South Twin - 2019 ==================================
stwin19 = temp %>%
  filter(lake_id == 406, year == 2019) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))

stwin19_heat <- ggplot(stwin19, aes(doy, depth_m, fill= dtemp)) + 
  geom_tile()+xlim(135,255)+
  scale_fill_viridis(option="D",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=10),legend.position="none",axis.title=element_text(size=12))


# Swan Lake - 2018 ==================================
swan18 = temp %>%
  filter(lake_id == 114, year == 2018) %>%
  group_by(doy, depth_m) %>%
  summarize(dtemp = mean(temp_c, na.rm = TRUE))

swan18_heat <- ggplot(swan18, aes(doy, depth_m, fill= dtemp)) + 
  geom_tile()+xlim(135,255)+
  scale_fill_viridis(option="D",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=10),legend.position="none",axis.title=element_text(size=12))

# windows(height = 9, width = 6)
# wrap_plots(center18_heat, center19_heat, 
#            fiveisl18_heat, fiveisl19_heat, 
#            stwin18_heat, stwin19_heat, 
#            swan18_heat,
#            ncol = 2, nrow = 4)



center18 = as.data.frame(center18)
center19 = as.data.frame(center19)
fiveisl18 = as.data.frame(fiveisl18)
fiveisl19 = as.data.frame(fiveisl19)
swan18 = as.data.frame(swan18)
stwin18 = as.data.frame(stwin18)
stwin19 = as.data.frame(stwin19)

brewer.pal(7,"Dark2")
CEcol<-"#A6761D" #brown for Center Lake
CEtrans <- rgb(166, 118, 29, max = 255, alpha = 150)
FIcol<-"#D95F02" #orange, Five Island
FItrans <- rgb(217, 95, 2, max = 255, alpha = 150)
NTcol<-"#7570B3" #purpleblue, North Twin
SIcol<-"#E7298A" #pink, Silver
SOcol<-"#E6AB02" #mustard, Storm
STcol<- "#1B9E77" #teal, South Twin
STtrans <- rgb(27, 158, 119, max = 255, alpha = 150)
SWcol<-"#66A61E" #green, Swan
SWtrans <- rgb(102, 166, 30, max = 255, alpha = 150)
GVcol <- "#e7298a"
GVtrans <- rgb(231, 41, 138, max = 255, alpha = 150)

windows(height = 8, width = 5)
par(mfrow = c(4,1), omi = c(0.3,0.5,0.1,0.1), mai = c(0.5,0.3,0,0.1))
#Five Island - 2018
plot(fiveisl18[fiveisl18$depth_m==0, "doy"],
     fiveisl18[fiveisl18$depth_m==0, "dtemp"] - 
       fiveisl18[fiveisl18$depth_m==6, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = FIcol, 
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(135, fiveisl18[fiveisl18$depth_m==0, "doy"], 255),
        c(0, fiveisl18[fiveisl18$depth_m==0, "dtemp"] - 
            fiveisl18[fiveisl18$depth_m==6, "dtemp"], 0), 
        col = FItrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.5, "Five Island\n(max depth = 6 m)")

#Center Lake - 2018
plot(center18[center18$depth_m==0, "doy"],
      center18[center18$depth_m==0, "dtemp"] - 
        center18[center18$depth_m==3.5, "dtemp"], 
      type = "l", lwd = 2, col = CEcol, 
     ylim = c(0,12), xlim = c(124,255), 
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(135, center18[center18$depth_m==0, "doy"], 255),
        c(0, center18[center18$depth_m==0, "dtemp"] - 
            center18[center18$depth_m==3.5, "dtemp"], 0), 
        col = CEtrans)
abline(1,0, lwd = 1, lty = 3)
text(230, 10, cex = 1.5, "Center Lake\n(max depth = 3.5 m)")


#Swan Lake - 2018
plot(swan18[swan18$depth_m==0, "doy"],
      swan18[swan18$depth_m==0, "dtemp"] - 
        swan18[swan18$depth_m==3, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = SWcol,
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(124, swan18[swan18$depth_m==0, "doy"], 254),
        c(0, swan18[swan18$depth_m==0, "dtemp"] - 
            swan18[swan18$depth_m==3, "dtemp"], 0), 
        col = SWtrans)
abline(1,0, lwd = 1, lty = 3)
mtext(side = 2, line = 3, "                                  Temperature Difference, Surface - Bottom")
text(230, 10, cex = 1.5, "Swan Lake\n(max depth = 3 m)")


# South Twin - 2018
plot(stwin18[stwin18$depth_m==0.5, "doy"],
      stwin18[stwin18$depth_m==0.5, "dtemp"] - 
        stwin18[stwin18$depth_m==1.5, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,255), col = STcol,
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(130, stwin18[stwin18$depth_m==0.5, "doy"], 255),
        c(0, stwin18[stwin18$depth_m==0.5, "dtemp"] - 
            stwin18[stwin18$depth_m==1.5, "dtemp"], 0), 
        col = STtrans)
abline(1,0, lwd = 1, lty = 3)
mtext(side = 1, line = 3, "Day of Year, 2018")
text(230, 10, cex = 1.5, "South Twin\n(max depth = 1.5 m)")



windows(height = 8, width = 5)
par(mfrow = c(4,1), omi = c(0.3,0.5,0.1,0.1), mai = c(0.5,0.3,0,0.1))
#Five Island - 2019
plot(fiveisl19[fiveisl19$depth_m==0, "doy"],
     fiveisl19[fiveisl19$depth_m==0, "dtemp"] - 
       fiveisl19[fiveisl19$depth_m==6, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,265), col = FIcol, 
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(135, fiveisl19[fiveisl19$depth_m==0, "doy"], 218),
        c(0, fiveisl19[fiveisl19$depth_m==0, "dtemp"] - 
            fiveisl19[fiveisl19$depth_m==6, "dtemp"], 0), 
        col = FItrans)
abline(1,0, lwd = 1, lty = 3)
text(235, 10, cex = 1.5, "Five Island\n(max depth = 6 m)")

#Center Lake - 2019
plot(center19[center19$depth_m==0, "doy"],
     center19[center19$depth_m==0, "dtemp"] - 
       center19[center19$depth_m==3.5, "dtemp"], 
     type = "l", lwd = 2, col = CEcol, 
     ylim = c(0,12), xlim = c(124,265), 
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(135, center19[center19$depth_m==0, "doy"], 170),
        c(0, center19[center19$depth_m==0, "dtemp"] - 
            center19[center19$depth_m==3.5, "dtemp"], 0), 
        col = CEtrans)
abline(1,0, lwd = 1, lty = 3)
text(235, 10, cex = 1.5, "Center Lake\n(max depth = 3.5 m)")


#Green Valley Lake - 2019
gvl19 = read.csv("GVL_Site4_Tchain.csv") %>%
  group_by(doy) %>%
  summarize(surface = mean(Temp_0, na.rm = TRUE),
            bottom = mean(Temp_6, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff_temp = surface - bottom)
gvl19 = as.data.frame(gvl19)

plot(gvl19$doy, gvl19$diff_temp, type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,265), col = GVcol,
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(134, gvl19$doy, 263),
        c(0, gvl19$diff_temp, 0), 
        col = GVtrans)
abline(1,0, lwd = 1, lty = 3)
mtext(side = 2, line = 3, "                                  Temperature Difference, Surface - Bottom")
text(235, 10, cex = 1.5, "Green Valley\n(max depth = 6 m)")


# South Twin - 2019
plot(stwin19[stwin19$depth_m==0.5, "doy"],
     stwin19[stwin19$depth_m==0.5, "dtemp"] - 
       stwin19[stwin19$depth_m==1.5, "dtemp"], type = "l", lwd = 2,
     ylim = c(0,12), xlim = c(124,265), col = STcol,
     xlab = "", ylab = "", cex.axis = 1.5)
polygon(c(130, stwin19[stwin19$depth_m==0.5, "doy"], 255),
        c(0, stwin19[stwin19$depth_m==0.5, "dtemp"] - 
            stwin19[stwin19$depth_m==1.5, "dtemp"], 0), 
        col = STtrans)
abline(1,0, lwd = 1, lty = 3)
mtext(side = 1, line = 3, "Day of Year, 2018")
text(235, 10, cex = 1.5, "South Twin\n(max depth = 1.5 m)")

