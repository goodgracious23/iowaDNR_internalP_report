setwd("C:/Users/grace/Box/Carp Project Iowa DNR/EDI Data Submission/Water Chemistry")

wq = read.csv("Carp_WQ_2018-2020.csv")

center_epi = wq %>% filter(lake_id==19, depth=="Epi")
center_hypo = wq %>% filter(lake_id==19, depth=="Hypo")
fiveisl_epi = wq %>% filter(lake_id==36, depth=="Epi")
fiveisl_hypo = wq %>% filter(lake_id==36, depth=="Hypo")
ntwin_epi = wq %>% filter(lake_id==90, depth=="Epi")
ntwin_hypo = wq %>% filter(lake_id==90, depth=="Hypo")
silver_epi = wq %>% filter(lake_id==105, depth=="Epi")
silver_hypo = wq %>% filter(lake_id==105, depth=="Hypo")
storm_epi = wq %>% filter(lake_id==113, depth=="Epi")
storm_hypo = wq %>% filter(lake_id==113, depth=="Hypo")
stwin_epi = wq %>% filter(lake_id==406, depth=="Epi")
stwin_hypo = wq %>% filter(lake_id==406, depth=="Hypo")

# Colors
brewer.pal(7,"Dark2")
CEcol<-"#A6761D" #brown for Center Lake
CEtrans <- rgb(166, 118, 29, max = 255, alpha = 100)
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
