# Succinct Code for Grace # 

# Set working directory to wherever .csv files are # 

# Dataset 1 = fitsdata_zp-miv_long_springsummer.csv
# Datset 2 = shortoutput_zp-miv_springsummer.csv

# Plotting Figure 2 #=====================
#### Size Spectrum analysis of all lakes with slopes ####
# Output graphs with linear fits # 
library(tidyverse)
library(lubridate)

fits = read_csv('fitsdata_zp-miv_long_springsummer.csv') 
short.output = read_csv('shortoutput_zp-miv_springsummer.csv') # plotting data 
output_for_plot = short.output %>% 
  left_join(., fits, by = c('year', 'lake'), all.x=T) %>%
  filter(!lake=="Blue")
output_for_plot
output_for_plot$lake.f = factor(output_for_plot$lake, levels = c('Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
output_for_plot$year.f = factor(output_for_plot$year, levels = c(2018, 2019, 2020))
output_for_plot$year.lake <- as.factor(paste(output_for_plot$year.f, output_for_plot$lake))

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


all_plot = 
  ggplot(output_for_plot, aes(BINMID_LOG, DENS_LOG, shape = DATAUSEDINFIT, 
                              color = year.lake)) + 
  geom_point(size = 2) +  scale_shape_manual(values = c(1,19)) +
  ylim (2, 22) + xlim (-32, 0) +
  labs( x = expression ( paste ( 'Log'[2], 'Dry Weight Biomass (g)') ), 
        y = expression ( paste ( 'Log'[2], 'Abundance (Individuals/m'^2,')' ) ),
        element_text(size = 14) ) +
  scale_color_manual(values = c(CEpost, FIpost, NTpost, SIpost, SOpost, STpost,
                                CEpost, FIpost, NTpost, SIpost, SOpost, STpost,
                                CEpost, FIpost, NTpost, SIpost, SOpost, STpost)) +
  theme_bw() + theme(legend.position = "none", 
                     plot.margin = unit(c(0.2,0.2,0.2,0.2),"in")) +
  geom_smooth( data = output_for_plot %>% filter(DATAUSEDINFIT == 'used') %>% 
                 subset ( fitmax & fitmin), 
               method = lm, se = FALSE)
windows(height = 5, width = 5)
all_plot


fits_dat = read_csv('fitsdata_zp-miv_long_springsummer.csv')
center.su_dat = fits_dat %>% filter(lake == 'Center') 
fiveisl.su_dat = fits_dat %>% filter(lake == 'Five.Island') 
ntwin.su_dat = fits_dat %>% filter(lake == 'North.Twin') 
silver.su_dat = fits_dat %>% filter(lake == 'Silver') 
storm.su_dat = fits_dat %>% filter(lake == 'Storm')
stwin.su_dat = fits_dat %>% filter(lake == 'South.Twin')

windows(height = 5, width = 4)
par(mfrow = c(3,2), mai = c(0.1,0.2,0.1,0.1), omi = c(0.5,0.5,0.1,0.1))
#Center Lake
plot(slope~year, data = center.su_dat, 
     pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), 
     cex = 3, col = c(CEpost, CEpost, CEpre), xaxt = 'n')
arrows(x0 = center.su_dat$year, y0 = center.su_dat$slp_l95ci, 
       x1 = center.su_dat$year, y1 = center.su_dat$slp_u95ci, 
       col = c(CEpost, CEpost, CEpre), code = 3, angle = 90, length = 0, lwd = 3)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)

#Five Island
plot(slope~year, data = fiveisl.su_dat, 
     pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), 
     cex = 3, col = c(FIpost, FIpost, FIpre), xaxt = 'n')
arrows(x0 = fiveisl.su_dat$year, y0 = fiveisl.su_dat$slp_l95ci, 
       x1 = fiveisl.su_dat$year, y1 = fiveisl.su_dat$slp_u95ci, 
       col = c(FIpost, FIpost, FIpre), code = 3, angle = 90, length = 0, lwd = 3)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)

#North Twin
plot(slope~year, data = ntwin.su_dat, 
     pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), 
     cex = 3, col = c(NTpre, NTpost, NTpost), xaxt = 'n')
arrows(x0 = ntwin.su_dat$year, y0 = ntwin.su_dat$slp_l95ci, 
       x1 = ntwin.su_dat$year, y1 = ntwin.su_dat$slp_u95ci, 
       col = c(NTpre, NTpost, NTpost), code = 3, angle = 90, length = 0, lwd = 3)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)

#Silver Lake
plot(slope~year, data = silver.su_dat, 
     pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), 
     cex = 3, col = c(SIpre, SIpost, SIpost), xaxt = 'n')
arrows(x0 = silver.su_dat$year, y0 = silver.su_dat$slp_l95ci, 
       x1 = silver.su_dat$year, y1 = silver.su_dat$slp_u95ci, 
       col = c(SIpre, SIpost, SIpost), code = 3, angle = 90, length = 0, lwd = 3)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)

#Storm Lake
plot(slope~year, data = storm.su_dat, 
     pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), 
     cex = 3, col = SOpre, xaxt = 'n')
arrows(x0 = storm.su_dat$year, y0 = storm.su_dat$slp_l95ci, 
       x1 = storm.su_dat$year, y1 = storm.su_dat$slp_u95ci, 
       col = SOpre, code = 3, angle = 90, length = 0, lwd = 3)
axis(side = 1, at = c(2018, 2019, 2020), labels = TRUE)
abline(h = -1, lty =3)

#South Twin
plot(slope~year, data = stwin.su_dat, 
     pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), 
     cex = 3, col = STpre, xaxt = 'n')
arrows(x0 = stwin.su_dat$year, y0 = stwin.su_dat$slp_l95ci, 
       x1 = stwin.su_dat$year, y1 = stwin.su_dat$slp_u95ci, 
       col = STpre, code = 3, angle = 90, length = 0, lwd = 3)
axis(side = 1, at = c(2018, 2019, 2020), labels = TRUE)
abline(h = -1, lty =3)

