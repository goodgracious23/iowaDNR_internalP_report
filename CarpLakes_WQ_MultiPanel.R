setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Water Chemistry")

wq = read.csv("Carp_WQ_2018-2020.csv")
wq$year.f <- as.factor(wq$year)
wq$year.depth <- as.factor(paste(wq$year.f, wq$depth))

library(tidyverse)
library(gridExtra)
library(hrbrthemes)
library(RColorBrewer)
brewer.pal(7,"Dark2")
CEpre<-"#A6761D" #brown for Center Lake
CEpost <- rgb(166, 118, 29, max = 255, alpha = 70)
FIpre<-"#D95F02" #orange, Five Island
FIpost <- rgb(217, 95, 2, max = 255, alpha = 70)
NTpre <-"#7570B3" #purpleblue, North Twin
NTpost <- rgb(117,112,179, max = 255, alpha = 70)
SIpre <-"#E7298A" #pink, Silver
SIpost <- rgb(231, 41, 138, max = 255, alpha = 70)
SOpre <-"#E6AB02" #mustard, Storm
SOpost <- rgb(230, 171, 2, max = 255, alpha = 70)
STpre <-"#1B9E77" #teal, South Twin
STpost <- rgb(27, 158, 119, max = 255, alpha = 70)


center = wq %>% filter(lake_name=="Center") 
fiveisl = wq %>% filter(lake_name=="Five Island")
ntwin = wq %>% filter(lake_name=="North Twin") 
silver = wq %>% filter(lake_name=="Silver") 
storm = wq %>% filter(lake_name=="Storm") 
stwin = wq %>% filter(lake_name=="South Twin") 


CE_srp <- ggplot(data = center, aes(x = year.depth, y = log10(srp_ugL))) +
        geom_boxplot(fill = c(CEpost, CEpost, CEpost, CEpost, CEpre, CEpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 2.5))

FI_srp <- ggplot(data = fiveisl, aes(x = year.depth, y = log10(srp_ugL))) +
        geom_boxplot(fill = c(FIpost, FIpost, FIpost, FIpost, FIpre, FIpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 2.5))

NT_srp <- ggplot(data = ntwin, aes(x = year.depth, y = log10(srp_ugL))) +
        geom_boxplot(fill = c(NTpre, NTpre, NTpost, NTpost, NTpost, NTpost), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 2.5))

SI_srp <- ggplot(data = silver, aes(x = year.depth, y = log10(srp_ugL))) +
        geom_boxplot(fill = c(SIpre, SIpre, SIpost, SIpost, SIpost, SIpost), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 2.5))

SO_srp <- ggplot(data = storm, aes(x = year.depth, y = log10(srp_ugL))) +
        geom_boxplot(fill = c(SOpre, SOpre, SOpre, SOpre, SOpre, SOpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 2.5))

ST_srp <- ggplot(data = stwin, aes(x = year.depth, y = log10(srp_ugL))) +
        geom_boxplot(fill = c(STpre, STpre, STpre, STpre, STpre, STpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0, 2.5))

# windows(height = 6, width = 8)
srp_carp <- grid.arrange(CE_srp, FI_srp, NT_srp,
                         SI_srp, SO_srp, ST_srp, nrow = 3)
ggsave("SRP_summary_carpLakes.png", srp_carp,
       width = 8, height = 6, units="in", dpi = 300)


center_iss = wq %>% filter(lake_name=="Center") %>% filter(!is.na(iss_mgL))
fiveisl_iss = wq %>% filter(lake_name=="Five Island") %>% filter(!is.na(iss_mgL))
ntwin_iss = wq %>% filter(lake_name=="North Twin")  %>% filter(!is.na(iss_mgL))
silver_iss = wq %>% filter(lake_name=="Silver")  %>% filter(!is.na(iss_mgL))
storm_iss = wq %>% filter(lake_name=="Storm")  %>% filter(!is.na(iss_mgL))
stwin_iss = wq %>% filter(lake_name=="South Twin")  %>% filter(!is.na(iss_mgL))

CE_iss <- ggplot(data = center_iss, aes(x = year.depth, y = log10(iss_mgL+1))) +
        geom_boxplot(fill = c(CEpost, CEpost, CEpost, CEpost, CEpre, CEpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0,3))

FI_iss <- ggplot(data = fiveisl_iss, aes(x = year.depth, y = log10(iss_mgL+1))) +
        geom_boxplot(fill = c(FIpost, FIpost, FIpost, FIpost, FIpre, FIpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0,3))

NT_iss <- ggplot(data = ntwin_iss, aes(x = year.depth, y = log10(iss_mgL+1))) +
        geom_boxplot(fill = c(NTpre, NTpre, NTpost, NTpost, NTpost, NTpost), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0,3))

SI_iss <- ggplot(data = silver, aes(x = year.depth, y = log10(iss_mgL+1))) +
        geom_boxplot(fill = c(SIpre, SIpre, SIpost, SIpost, SIpost, SIpost), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0,3))

SO_iss <- ggplot(data = storm, aes(x = year.depth, y = log10(iss_mgL+1))) +
        geom_boxplot(fill = c(SOpre, SOpre, SOpre, SOpre, SOpre, SOpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0,3))

ST_iss <- ggplot(data = stwin_iss, aes(x = year.depth, y = log10(iss_mgL+1))) +
        geom_boxplot(fill = c(STpre, STpre, STpre, STpre, STpre, STpre), 
                     outlier.size = 0) +
        geom_jitter(color = rgb(77,77,77, max = 255, alpha = 100), 
                    size = 1.5, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none", 
              plot.margin = unit(c(0.2,0.2,0.1,0.1),"in")) +
        xlab("") + ylab("") + coord_cartesian(ylim = c(0,3))

# windows(height = 6, width = 8)
iss_carp <- grid.arrange(CE_iss, FI_iss, NT_iss,
                        SI_iss, SO_iss, ST_iss, nrow = 3)
ggsave("ISS_summary_carpLakes.png", iss_carp,
       width = 8, height = 6, units="in", dpi = 300)



center_epi = wq %>% filter(lake_id==19, depth=="Epi", program=="Carp")
center_hypo = wq %>% filter(lake_id==19, depth=="Hypo", program=="Carp")
center_alm = wq %>% filter(lake_id==19, program=="ALM")

fiveisl_epi = wq %>% filter(lake_id==36, depth=="Epi", program=="Carp")
fiveisl_hypo = wq %>% filter(lake_id==36, depth=="Hypo", program=="Carp")
fiveisl_alm = wq %>% filter(lake_id==36, program=="ALM")

ntwin_epi = wq %>% filter(lake_id==90, depth=="Epi", program=="Carp")
ntwin_hypo = wq %>% filter(lake_id==90, depth=="Hypo", program=="Carp")
ntwin_alm = wq %>% filter(lake_id==90, program=="ALM")

silver_epi = wq %>% filter(lake_id==105, depth=="Epi", program=="Carp")
silver_hypo = wq %>% filter(lake_id==105, depth=="Hypo", program=="Carp")
silver_alm = wq %>% filter(lake_id==105, program=="ALM")

storm_epi = wq %>% filter(lake_id==113, depth=="Epi", program=="Carp")
storm_hypo = wq %>% filter(lake_id==113, depth=="Hypo", program=="Carp")
storm_alm = wq %>% filter(lake_id==113, program=="ALM")

stwin_epi = wq %>% filter(lake_id==406, depth=="Epi", program=="Carp")
stwin_hypo = wq %>% filter(lake_id==406, depth=="Hypo", program=="Carp")
stwin_alm = wq %>% filter(lake_id==406, program=="ALM")


# Colors
brewer.pal(7,"Dark2")
CEcol<-"#A6761D" #brown for Center Lake
CEtrans <- rgb(166, 118, 29, max = 255, alpha = 100)
CEalm <- rgb(166, 118, 29, max = 255, alpha = 150)
FIcol<-"#D95F02" #orange, Five Island
FItrans <- rgb(217, 95, 2, max = 255, alpha = 100)
NTcol<-"#7570B3" #purpleblue, North Twin
NTtrans <- rgb(117,112,179, max = 255, alpha = 100)
SIcol<-"#E7298A" #pink, Silver
SItrans <- rgb(231, 41, 138, max = 255, alpha = 100)
SOcol<-"#E6AB02" #mustard, Storm
SOtrans <- rgb(230, 171, 2, max = 255, alpha = 100)
STcol<- "#1B9E77" #teal, South Twin
STtrans <- rgb(27, 158, 119, max = 255, alpha = 100)


# SOLUBLE REACTIVE PHOSPHORUS =======================================
windows(height = 4.5, width = 6.5)
par(mfrow = c(2,3), omi = c(0.2,0.5,0.3,0.1), mai = c(0.3,0.1,0.1,0.1))

plot(center_epi$year_frac, log10(center_epi$srp_ugL), 
     col = CEtrans, pch = 19, cex = 2, ylim = c(log10(2), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(center_hypo$year_frac, log10(center_hypo$srp_ugL), 
       col = CEcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Center")
axis(side = 2, at = c(log10(2), log10(3), log10(4), log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200), log10(300)),
     labels = c("", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", "", ""))

plot(fiveisl_epi$year_frac, log10(fiveisl_epi$srp_ugL), 
     col = FItrans, pch = 19, cex = 2, ylim = c(log10(2), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(fiveisl_hypo$year_frac, log10(fiveisl_hypo$srp_ugL), 
       col = FIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Five Island")
axis(side = 2, at = c(log10(2), log10(3), log10(4), log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200), log10(300)),
     labels = c("", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", "", ""))

plot(ntwin_epi$year_frac, log10(ntwin_epi$srp_ugL), 
     col = NTtrans, pch = 19, cex = 2, ylim = c(log10(2), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(ntwin_hypo$year_frac, log10(ntwin_hypo$srp_ugL), 
       col = NTcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "North Twin")
axis(side = 2, at = c(log10(2), log10(3), log10(4), log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200), log10(300)),
     labels = c("", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", "", ""))

plot(silver_epi$year_frac, log10(silver_epi$srp_ugL), 
     col = SItrans, pch = 19, cex = 2, ylim = c(log10(2), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(silver_hypo$year_frac, log10(silver_hypo$srp_ugL), 
       col = SIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Silver (Dickinson)")
axis(side = 2, at = c(log10(2), log10(3), log10(4), log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200), log10(300)),
     labels = c("", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", "", ""))

plot(storm_epi$year_frac, log10(storm_epi$srp_ugL), 
     col = SOtrans, pch = 19, cex = 2, ylim = c(log10(2), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(storm_hypo$year_frac, log10(storm_hypo$srp_ugL), 
       col = SOcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Storm")
axis(side = 2, at = c(log10(2), log10(3), log10(4), log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200), log10(300)),
     labels = c("", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", "", ""))

plot(stwin_epi$year_frac, log10(stwin_epi$srp_ugL), 
     col = STtrans, pch = 19, cex = 2, ylim = c(log10(2), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(stwin_hypo$year_frac, log10(stwin_hypo$srp_ugL), 
       col = STcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "South Twin")
axis(side = 2, at = c(log10(2), log10(3), log10(4), log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200), log10(300)),
     labels = c("", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", "", ""))


# SUSPENDED SOLIDS =================================================
windows(height = 4.5, width = 6.5)
par(mfrow = c(2,3), omi = c(0.2,0.5,0.3,0.1), mai = c(0.3,0.1,0.1,0.1))

plot(center_epi$year_frac, log10(center_epi$tss_mgL), 
     col = CEtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(center_hypo$year_frac, log10(center_hypo$tss_mgL), 
       col = CEcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Center")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(fiveisl_epi$year_frac, log10(fiveisl_epi$tss_mgL), 
     col = FItrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(fiveisl_hypo$year_frac, log10(fiveisl_hypo$tss_mgL), 
       col = FIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Five Island")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(ntwin_epi$year_frac, log10(ntwin_epi$tss_mgL), 
     col = NTtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(ntwin_hypo$year_frac, log10(ntwin_hypo$tss_mgL), 
       col = NTcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "North Twin")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(silver_epi$year_frac, log10(silver_epi$tss_mgL), 
     col = SItrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(silver_hypo$year_frac, log10(silver_hypo$tss_mgL), 
       col = SIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Silver (Dickinson)")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(storm_epi$year_frac, log10(storm_epi$tss_mgL), 
     col = SOtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(storm_hypo$year_frac, log10(storm_hypo$tss_mgL), 
       col = SOcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Storm")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(stwin_epi$year_frac, log10(stwin_epi$tss_mgL), 
     col = STtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(stwin_hypo$year_frac, log10(stwin_hypo$tss_mgL), 
       col = STcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "South Twin")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))



# SUSPENDED SOLIDS =================================================
windows(height = 4.5, width = 6.5)
par(mfrow = c(2,3), omi = c(0.2,0.5,0.3,0.1), mai = c(0.3,0.1,0.1,0.1))

plot(center_epi$year_frac, log10(center_epi$tss_mgL), 
     col = CEtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(center_hypo$year_frac, log10(center_hypo$tss_mgL), 
       col = CEcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Center")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(fiveisl_epi$year_frac, log10(fiveisl_epi$tss_mgL), 
     col = FItrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(fiveisl_hypo$year_frac, log10(fiveisl_hypo$tss_mgL), 
       col = FIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Five Island")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(ntwin_epi$year_frac, log10(ntwin_epi$tss_mgL), 
     col = NTtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(ntwin_hypo$year_frac, log10(ntwin_hypo$tss_mgL), 
       col = NTcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "North Twin")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(silver_epi$year_frac, log10(silver_epi$tss_mgL), 
     col = SItrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(silver_hypo$year_frac, log10(silver_hypo$tss_mgL), 
       col = SIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Silver (Dickinson)")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(storm_epi$year_frac, log10(storm_epi$tss_mgL), 
     col = SOtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(storm_hypo$year_frac, log10(storm_hypo$tss_mgL), 
       col = SOcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Storm")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(stwin_epi$year_frac, log10(stwin_epi$tss_mgL), 
     col = STtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(stwin_hypo$year_frac, log10(stwin_hypo$tss_mgL), 
       col = STcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "South Twin")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))



#Inorganic Suspended Solids BOXPLOTS =================================

        ggplot( aes(x = regime, y = period, fill = regime)) +
        geom_boxplot(fill = c(STtrans, SItrans), outlier.size = 0) +
        geom_jitter(color="black", size = 4, alpha = 0.5, width = 0.1) +
        theme_ipsum() +
        theme(legend.position = "none") +
        xlab("")

center_periods

plot(center_epi$year_frac, log10(center_epi$tss_mgL), 
     col = CEtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(center_hypo$year_frac, log10(center_hypo$tss_mgL), 
       col = CEcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Center")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(fiveisl_epi$year_frac, log10(fiveisl_epi$tss_mgL), 
     col = FItrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(fiveisl_hypo$year_frac, log10(fiveisl_hypo$tss_mgL), 
       col = FIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Five Island")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(ntwin_epi$year_frac, log10(ntwin_epi$tss_mgL), 
     col = NTtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("","",""))
points(ntwin_hypo$year_frac, log10(ntwin_hypo$tss_mgL), 
       col = NTcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "North Twin")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(silver_epi$year_frac, log10(silver_epi$tss_mgL), 
     col = SItrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(silver_hypo$year_frac, log10(silver_hypo$tss_mgL), 
       col = SIcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Silver (Dickinson)")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(storm_epi$year_frac, log10(storm_epi$tss_mgL), 
     col = SOtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(storm_hypo$year_frac, log10(storm_hypo$tss_mgL), 
       col = SOcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "Storm")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

plot(stwin_epi$year_frac, log10(stwin_epi$tss_mgL), 
     col = STtrans, pch = 19, cex = 2, ylim = c(log10(1), log10(300)),
     xaxt = "n", yaxt = "n", xlim = c(2018.3, 2020.9))
axis(side = 1, at = c(2018.5, 2019.5, 2020.5), labels = c("2018","2019","2020"))
points(stwin_hypo$year_frac, log10(stwin_hypo$tss_mgL), 
       col = STcol, pch = 15, cex = 1.5)
mtext(side = 3, line = 0.5, "South Twin")
axis(side = 2, at = c(log10(1), log10(2), log10(3), log10(4), 
                      log10(5), log10(6),
                      log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), 
                      log10(60), log10(70), log10(80), log10(90), 
                      log10(100), log10(200)),
     labels = c("","", "", "", "", "", "", "", "", "10", "", "", "", "","","","","","100", ""))

