nuts = read.csv("nutrients_gvl_2019.csv")

surface = nuts %>% filter(depthID==1) %>% mutate(siteCat = as.factor(siteID))
bottom = nuts %>% filter(depthID==4) %>% mutate(siteCat = as.factor(siteID))

surface_ridge = 
  ggplot(surface, aes(x = srp, y = siteCat, fill = siteCat)) + 
  geom_density_ridges2(scale = 2, linetype = 0, panel_scaling = TRUE) +
  scale_fill_cyclical(values = c("#bdd7e7", "#6baed6", "#3182bd", "#08519c", "#6baed6","#bdd7e7")) +
  theme_ridges() +
  coord_cartesian(xlim = c(-150, 1200)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Surface') + xlab(label = '') + ylab(label = '')

bottom_ridge = 
  ggplot(bottom, aes(x = srp, y = siteCat, fill = siteCat)) + 
  geom_density_ridges2(scale = 2, linetype = 0, panel_scaling = TRUE) +
  scale_fill_cyclical(values = c("#bdd7e7", "#6baed6", "#3182bd", "#08519c", "#6baed6","#bdd7e7")) +
  theme_ridges() +
  coord_cartesian(xlim = c(-150, 1200)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Bottom') + xlab(label = 'Soluble Reactive P (ug/L)') + ylab(label = '')

grid.arrange(surface_ridge, bottom_ridge, nrow = 2)
