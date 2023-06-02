# ALM Profiles
# Round 1 ===================================
surf_almR1 = read.csv("ALM2018R1_Profiles_Final_v3.csv") %>%
  rename(surf_depth = Depth..m.,
         surf_do_sat = DO....Saturation.,
         surf_do_mgL = DO..mg.L.,
         surf_temp = Temperature..Deg.C.) %>%
  group_by(Lake) %>%
  filter(surf_depth == 0) %>%
  select(Lake, surf_depth, surf_do_sat, surf_do_mgL, surf_temp)


hypo_almR1 = read.csv("ALM2018R1_Profiles_Final_v3.csv") %>%
  rename(btm_depth = Depth..m.,
         btm_do_sat = DO....Saturation.,
         btm_do_mgL = DO..mg.L.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(Lake) %>%
  filter(btm_depth == max(btm_depth)) %>%
  select(Lake, btm_depth, btm_do_sat, btm_do_mgL, btm_temp)

top_btmR1 = left_join(surf_almR1, hypo_almR1, by = c("Lake")) %>%
  mutate(diff_do_sat = surf_do_sat - btm_do_sat,
         diff_do_mgL = surf_do_mgL - btm_do_mgL,
         diff_temp = surf_temp - btm_temp)

# Round 2 ===================================
surf_almR2 = read.csv("ALM2018R2_Profiles_Final.csv") %>%
  rename(surf_depth = Depth..m.,
         surf_do_sat = DO....Saturation.,
         surf_do_mgL = DO..mg.L.,
         surf_temp = Temperature..Deg.C.) %>%
  group_by(Lake) %>%
  filter(surf_depth == 0) %>%
  select(Lake, surf_depth, surf_do_sat, surf_do_mgL, surf_temp)


hypo_almR2 = read.csv("ALM2018R2_Profiles_Final.csv") %>%
  rename(btm_depth = Depth..m.,
         btm_do_sat = DO....Saturation.,
         btm_do_mgL = DO..mg.L.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(Lake) %>%
  filter(btm_depth == max(btm_depth)) %>%
  select(Lake, btm_depth, btm_do_sat, btm_do_mgL, btm_temp)

top_btmR2 = left_join(surf_almR2, hypo_almR2, by = c("Lake")) %>%
  mutate(diff_do_sat = surf_do_sat - btm_do_sat,
         diff_do_mgL = surf_do_mgL - btm_do_mgL,
         diff_temp = surf_temp - btm_temp)


# Round 3 ===================================
surf_almR3 = read.csv("ALM2018R3_Profiles_Final.csv") %>%
  rename(surf_depth = Depth..m.,
         surf_do_sat = DO....Saturation.,
         surf_do_mgL = DO..mg.L.,
         surf_temp = Temperature..Deg.C.) %>%
  group_by(Lake) %>%
  filter(surf_depth == 0) %>%
  select(Lake, surf_depth, surf_do_sat, surf_do_mgL, surf_temp)


hypo_almR3 = read.csv("ALM2018R3_Profiles_Final.csv") %>%
  rename(btm_depth = Depth..m.,
         btm_do_sat = DO....Saturation.,
         btm_do_mgL = DO..mg.L.,
         btm_temp = Temperature..Deg.C.) %>%
  group_by(Lake) %>%
  filter(btm_depth == max(btm_depth)) %>%
  select(Lake, btm_depth, btm_do_sat, btm_do_mgL, btm_temp)

top_btmR3 = left_join(surf_almR3, hypo_almR3, by = c("Lake")) %>%
  mutate(diff_do_sat = surf_do_sat - btm_do_sat,
         diff_do_mgL = surf_do_mgL - btm_do_mgL,
         diff_temp = surf_temp - btm_temp)

# PLOTTING ======================================
windows(height = 4, width = 3)
par(mfrow = c(3,1), omi = c(0.5,0.5,0.1,0.1), mai = c(0.2,0.2,0.1,0.1))

#Round 1 - 48% above 20% saturation
hist(top_btmR1$btm_do_sat, col = "#1b9e77", border = NA,
     ylim = c(0,44), xlim = c(0,150), breaks = 26, main = "")
text(100,30, "Late Spring -\n52% hypoxic", cex = 1.25)
polygon(c(0,20,20,0), c(0,0,44,44), 
        col = rgb(102,102,102, max = 255, alpha = 50), border = NA)
#Round 2 - 23% above 20% saturation
hist(top_btmR2$btm_do_sat, col = "#e7298a", border = NA,
     ylim = c(0,44), xlim = c(0,150), breaks = 20, main = "")
text(100,30, "Mid-Summer -\n77% hypoxic", cex = 1.25)
polygon(c(0,20,20,0), c(0,0,44,44), 
        col = rgb(102,102,102, max = 255, alpha = 50), border = NA)
mtext(side = 2, line = 3, "Number of Lakes", cex = 1)
#Round 3 - 36% above 20% saturation
hist(top_btmR3$btm_do_sat, col = "#e6ab02", border = NA, 
     ylim = c(0,44), xlim = c(0,150), breaks = 20, main = "")
text(100,30, "Late Summer - \n64% hypoxic", cex = 1.25)
polygon(c(0,20,20,0), c(0,0,44,44), 
        col = rgb(102,102,102, max = 255, alpha = 50), border = NA)
mtext(side = 1, line = 3, "Bottom DO % Saturation", cex = 1)

windows(height = 4, width = 4)
par(mfrow = c(1,1), omi = c(0.5,0.5,0.1,0.1), mai = c(0.2,0.2,0.1,0.1))
plot(top_btmR1$diff_temp, top_btmR1$btm_do_sat, cex = 1.25, pch = 19, 
     col = rgb(27,158,119, max = 255, alpha = 150), xlim = c(-1,28))
polygon(c(-0.5,28,28,-0.5), c(0,0,20,20), 
        col = rgb(102,102,102, max = 255, alpha = 70), border = NA)
polygon(c(-0.5,3,3,-0.5), c(0,0,160,160), 
        col = rgb(102,102,102, max = 255, alpha = 50), border = NA)
text(24,10, "hypoxic", cex = 1)
text(1.3,145, "mixed", cex = 1)
points(top_btmR2$diff_temp, top_btmR2$btm_do_sat, cex = 1.25, pch = 19, 
       col = rgb(231,41,138, max = 255, alpha = 150))
points(top_btmR3$diff_temp, top_btmR3$btm_do_sat, cex = 1.25, pch = 19,
       col = rgb(230,171,2, max = 255, alpha = 150))
mtext(side = 1, line = 2.2, "Temperature Difference, Surface to Bottom")
mtext(side = 2, line = 2.2, "Hypolimnion DO % Saturation")
#create a hypoxia frequency histogram