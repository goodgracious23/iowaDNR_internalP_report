# Project: Technical Report for Iowa Department of Natural Resources (IDNR)
# Title: Internal Phosphorus Loading in Iowa Lakes: Causes, Consequences, and Management Recommendations
#        Part 1: Phosphorus-Cycling Dynamics and Dominant Pathways in Iowa Lakes
# Last modified: 14 October 2022
# Contributors: Ellen Albright, Grace Wilkinson
# Description: Analysis code to summarize phosphorus dynamics in Iowa lakes and reservoirs and compare to a national survey

# Data citations: 
#   1. [NLA 2017] U.S. Environmental Protection Agency. 2022. National Aquatic Resource Surveys. National Lakes Assessment 2017 (data and metadata files). Available from U.S. EPA web page: https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys. Date accessed: 2022-10-13.
#   2. [IDNR, ALM] Iowa Department of Natural Resources. 2022. 21IOWA: Iowa DNR Surface Water Monitoring Data (data and metadata files). Available from https://programs.iowadnr.gov/aquia/Facilities/6/Detail Iowa DNR AQuIA Database. Date accessed: 2022-10-05.
#   3. [TMDL] Estimates of internal phosphorus loads were extracted by E. Albright from published models of total maximum daily load (TMDL) analyses. TMDL documents are available from the IDNR's ADBNet at https://programs/iowadnr.gov/adbnet/Docs/TMDL  

# The code can be run with the following data sets (in order first used in script)
#   1. "tmdl_data_ia.csv" - estimates of internal phosphorus loads extracted from published total maximum daily load (TMDL) models
#   2. "nla_2017_water_chemistry.csv" - water chemistry results of 2017 National Lakes Survey (NLA 2017), used total phosphorus concentrations
#   3. "ALM Data 2000-2021 WIDE.csv" - water chemistry results from Ambient Lake Monitoring Program (IDNR, ALM) 2000-2021
#   4. "ALM Lakes Static Characteristics Table.csv" - static lake features for ALM water bodies (e.g., location, morphometry) compiled by G. Wilkinson
#   5. "ALM Joined Data.csv" - combined file of recent ALM water chemistry data and static lake features (joined in this script)
#   6. "county.shp" - shapefile (spatial data) of Iowa country outlines to map spatial patterns in lake phosphorus data

# Packages needed to run the script (download as needed)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(gridExtra)

# Set up working directory - update to fit your system
getwd()
setwd('C:/Users/Ellen/Desktop/Box Sync/POSTDOC/IDNR/New_Analyses')

# Part 1 - Analysis of internal phosphorus loading estimates from TMDL Analyses --------------------------------------------------------------------------
# Read in TMDL data set
tmdl<-read.csv("tmdl_data_ia.csv")

# Summarize (median, mean) the proportion of total phosphorus loads that is due to internal loading (based on TMDL model estimates)
median(tmdl$internal_percent)
mean(tmdl$internal_percent)

# Figures: histograms of total phosphorus loads, internal phosphorus loads, and internal loading as a percent of the total load (based on TMDL model estimates)
total_load<-ggplot(data=tmdl, aes(x=total_kgyear))+
  geom_histogram(binwidth = 1000, color = "white", fill = "cadetblue4") +
  xlab("") +
  # xlab(bquote('Total Phosphorus Load (kg ' *year^-1*')')) + 
  ylab("Number of Waterbodies") +
  theme(axis.text = element_text(color = "black", size = 10),
        legend.position = "none",
        axis.title = element_text(size = 12))+
  theme_linedraw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

internal_load<-ggplot(data = tmdl, aes(x = internal_kgyear)) +
  geom_histogram(binwidth = 500, color = "white",fill = "cadetblue3") +
  xlab("") +
  # xlab(bquote('Internal Phosphorus Load (kg ' *year^-1*')')) + 
  ylab("Number of Waterbodies") +
  theme(axis.text = element_text(color = "black",size = 10),
        legend.position = "none",axis.title = element_text(size = 12)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

internal_percent<-ggplot(data = tmdl, aes(x = internal_percent)) +
  geom_histogram(binwidth = 5, color = "white",fill = "cadetblue3") +
  xlab("") +
  # xlab('Internal Load (% Total)') + 
  ylab("Number of Waterbodies") +
  theme(axis.text = element_text(color = "black", size = 10),
        legend.position = "none",
        axis.title = element_text(size = 12)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

internal_percent_histogram <-
  ggplot(tmdl, aes(y = internal_percent, x = eco_type, group = eco_type)) +
  geom_boxplot(aes(fill = eco_type), show.legend = FALSE) +
  geom_jitter(aes(fill = eco_type), show.legend = FALSE, 
              pch = 21, color = "black", size = 4) +
  scale_fill_manual(values = c("#5ab4ac","#d8b365","#f6e8c3")) +
  xlab(' ') + ylab("Internal Load (% Total)") +
  theme(axis.text.y = element_text(color="black", size = 12),
        legend.position = "none",
        axis.title = element_text(size = 12)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
internal_percent_histogram

windows(height = 5, width = 6.5)
tmdl_results<-grid.arrange(total_load, internal_load,
                           internal_percent, internal_percent_histogram,
                           layout_matrix = rbind(c(1, 2),
                                                 c(3, 4)))

# ggsave("tmdl histograms.png", tmdl_results, width=8, height=7, units="in", dpi=300)


# Explore relationships between internal phosphorus loads (based on TMDL model estimates) and lake features (depth, area, watershed ratio, volume, WRT)
# no patterns - except with WRT (water residence time)
plot(internal_percent~z_max_m, data=tmdl)
plot(internal_kgyear~z_max_m, data=tmdl)
plot(internal_percent~z_mean_m, data=tmdl)
plot(internal_kgyear~z_mean_m, data=tmdl)
plot(internal_percent~watershed_ratio, data=tmdl)
plot(internal_kgyear~watershed_ratio, data=tmdl)
plot(internal_percent~volume_km3, data=tmdl)
plot(internal_kgyear~volume_km3, data=tmdl)
plot(internal_percent~WRT_d, data=tmdl)
plot(internal_kgyear~WRT_d, data=tmdl)

# Figure: Relationship between internal phosphorus loading and WRT
tmdl$WRT_d<-as.numeric(tmdl$WRT_d)
ggplot(tmdl, aes(x=WRT_d, y=internal_percent))+
  geom_vline(xintercept=365, lty=2, lwd=1, color="grey50")+
  geom_vline(xintercept=30, lty=3, lwd=1, color="grey50")+
  geom_point(size=4)+
  xlab('Mean Water Residence Time (days)') + ylab("Internal Load (% Total)")+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggsave("WRT figure.png", width=6, height=5, units="in", dpi=300)

# Figure: Internal phosphorus loading across different water body types (glacial lake, reservoir, oxbow lake)
ggplot(tmdl, aes(y=internal_percent, x=eco_type, group=eco_type))+
  geom_boxplot(aes(fill=eco_type))+
  geom_jitter(aes(fill=eco_type),pch=21,color="black", size=3)+
  scale_fill_manual(values=c("#5ab4ac","#d8b365","#f6e8c3"))+
  xlab(' ') + ylab("Internal Load (% Total)")+
  theme(axis.text.y=element_text(color="black",size=11),legend.position="none",axis.title=element_text(size=11))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggsave("internal P by ecotype figure.png", width=6, height=5, units="in", dpi=300)


# Part 2 - Comparison of Iowa total phosphorus concentrations (IDNR, ALM) to national lakes assessment (NLA 2017) ------------------------------------
# Read in NLA survey data (NLA 2017)
nla<-read.csv("nla_2017_water_chemistry.csv")
nla_TP<-nla %>% 
  filter(ANALYTE=="PTL") # filter for total phosphorus concentrations only
# ensure that total phosphorus concentrations are coded as a number and filter out concentrations greater than 1 mg/L
nla_TP$TP_ugL<-as.numeric(nla_TP$RESULT)
nla_TP<-nla_TP %>% 
  filter(TP_ugL<1000)

# Summarize NLA 2017 total phosphorus values
plot(density(nla_TP$TP_ugL))
range(nla_TP$TP_ugL) # range = 0.238-978.675 ug/L
mean(nla_TP$TP_ugL) # mean = 83.33226 ug/L
median(nla_TP$TP_ugL) # median = 32.2155
length(unique(nla$UID)) # 1186 lakes

# Read in ALM water chemistry data (IDNR, ALM)
alm<-read.csv("ALM Data 2000-2021 WIDE.csv")
alm_5<-filter(alm, year>2016) # select the most recent 5 years of data
alm_5<-filter(alm_5, siteID!="14000450")

# Calculate annual mean total phosphorus for each lake, 2017-2021
alm5_annual_mean<-alm_5 %>% 
  group_by(siteID, year) %>% 
  summarise(across(c(TP, SRP, chlorophyll), ~mean(., na.rm=T))) %>%
  mutate(TP_ugL=TP*1000, SRP_ugL=SRP*1000) %>% 
  ungroup()

# Calculate the mean and standard error of the mean (SEM) for each lake over that period (mean and SEM calculated for annual average)
alm_5year_sum<-alm5_annual_mean %>% 
  group_by(siteID) %>% 
  mutate(meanTP_ugL=mean(TP_ugL), semTP_ugL=(sd(TP_ugL))/sqrt(5),
         meanSRP_ugL=mean(SRP_ugL), semSRP_ugL=(sd(SRP_ugL))/sqrt(5)) %>% 
  select(siteID, meanTP_ugL, semTP_ugL, meanSRP_ugL, semSRP_ugL) %>% 
  slice(n=1) %>% 
  ungroup()

# Summarize the 5-year mean total phosphorus concentrations for the ALM lakes
range(alm_5year_sum$meanTP_ugL) # 19 to 667.33
mean(alm_5year_sum$meanTP_ugL) # 100.64
median(alm_5year_sum$meanTP_ugL) # 87.73
length(unique(alm_5year_sum$siteID)) #137

# FIGURE: DISTRIBUTION OF TP (NLA 2017 VS ALM 2017-2021)
ggplot() + 
  geom_density(data=nla_TP, aes(x=TP_ugL, y = ..density..),
               lwd = 1, colour = "#1f78b4", fill = "#a6cee3", alpha = 0.35)+
  geom_density(data=alm_5year_sum, aes(x=meanTP_ugL, y = ..density..),
               lwd = 1, colour = "#33a02c", fill = "#b2df8a", alpha = 0.35)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab(bquote('Total Phosphorus (µg ' *L^-1*')')) + ylab("Density")+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))
ggsave("NLA_ALM_TP.png", width=7, height=5, units="in", dpi=300)


# Part 3 - Visualize total phosphorus concentrations in Iowa lakes and reservoirs (IDNR, ALM) ------------------------------------------------------
# Read in static features for all ALM water bodies
alm_stat<-read.csv("ALM Lakes Static Characteristics Table.csv")
# Join the data frame of static features with the ALM water chemistry data
alm_full<-merge(alm_stat, alm_5year_sum, by="siteID")
write.csv(alm_full, "ALM Joined Data.csv") # write a new CSV of the joined data frames. MANUALLY UPDATE SILVER LAKES TO INCLUDE CO. NAME! SPACE AFTER HAWTHORNE
alm_join<-read.csv("ALM Joined Data.csv") # read in the new, joined file

# Figure: Rank plot of Iowa water bodies by total phosphorus concentrations
alm_join %>%
  arrange(lakeName) %>%
  mutate(lakeName = factor(lakeName, levels=c("West Okoboji Lake", "Rudd Lake", "Arrowhead Lake", "Nine Eagles Lake", "Slip Bluff Lake",
                                              "Little Sioux Park Lake", "Mormon Trail Lake", "Kent Park Lake", "Willow Lake", "Lake Wapello",
                                              "Poll Miller Park Lake", "Lost Grove Lake", "Ada Hayden", "Pleasant Creek Lake", "Central Park Lake",
                                              "Green Castle Lake","Easter Lake","Yellow Smoke Park Lake","Lacey Keosauqua Park Lake","Lake MacBride",
                                              "Green Belt Lake","Cold Springs Lake","Nelson Park Lake","Lost Island Lake","Moorhead Park Pond",
                                              "Lake Cornelia","Lake Anita","Viking Lake","Mitchell Lake","Dog Creek (Lake)","Hawthorne Lake",
                                              "Springbrook Lake","Mariposa Lake","Belva Deer Lake","Nodaway Lake","Rathbun Reservoir",
                                              "Red Haw Lake","Crawford Creek Impoundment","Brushy Creek Lake","Three Mile Lake","Clear Lake",
                                              "Lake Sugema","Lake Ahquabi","Fogle Lake S.W.A.","Center Lake","George Wyth Lake",
                                              "East Okoboji Lake","Lake Meyer","Minnewashta Lake","Lake Icaria","Otter Creek Lake",
                                              "Briggs Woods Lake","Upper Gar Lake","Mill Creek Lake","Hickory Grove Lake","Hooper Area Pond",
                                              "Lower Gar Lake","Twelve Mile Creek Lake","Big Spirit Lake","Little River Watershed Lake","Big Creek Lake",
                                              "Eldred Sherwood Lake","Prairie Rose Lake","West Lake (Osceola)","Diamond Lake","Frog Hollow",
                                              "Greenfield Lake","Lake Manawa","Iowa Lake","North Twin Lake","Oldham Lake",
                                              "Lake of the Hills","Windmill Lake","Carter Lake","Upper Pine Lake","Wilson Park Lake",
                                              "Spring Lake","Five Island Lake","Lake of Three Fires","Lake Hendricks","South Prairie Lake",
                                              "Lake Keomah","Little Wall Lake","Badger Lake","Meadow Lake","Thayer Lake",
                                              "Roberts Creek Lake","Lower Pine Lake","Geode Lake","Arbor Lake","Arrowhead Pond",
                                              "Casey Lake","Beeds Lake","Blue Lake","Don Williams Lake","Crystal Lake",
                                              "Beaver Lake","Storm Lake (incl Little Storm Lake)","Lake Miami","Indian Lake","Big Hollow Lake",
                                              "Snyder Bend Lake","Hannen Lake","Black Hawk Lake","Rock Creek Lake","Silver Lake (Dickinson)","Silver Lake (Delaware)",
                                              "White Oak Conservation Area Lake","Ingham Lake","Meyer Lake","Lake Smith","Rodgers Park Lake",
                                              "Williamson Pond","Union Grove Lake","Lake Pahoja","Badger Creek Lake","Lake Darling",
                                              "DeSoto Bend","Green Valley Lake","Browns Lake","Ottumwa Lagoon","Silver Lake (Palo Alto)","Pierce Creek Pond",
                                              "Swan Lake","Manteno Park Pond","Littlefield Lake","Summit Lake","Avenue of the Saints Pond",
                                              "East Lake (Osceola)","Little Spirit Lake","Bob White Lake","Orient Lake"))) %>%
  ggplot(aes(x=meanTP_ugL,y=lakeName)) +
  annotate("rect", xmin = 12, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#1d91c0", alpha = 0.7)+
  annotate("rect", xmin = 24, xmax = 96, ymin = -Inf, ymax = Inf, fill = "#7fcdbb", alpha = 0.8)+
  annotate("rect", xmin = 96, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#c7e9b4", alpha = 0.8)+
  geom_point(size=1.8)+
  geom_errorbar(aes(y=lakeName, xmin=meanTP_ugL-semTP_ugL, xmax=meanTP_ugL+semTP_ugL),width=0,position=position_dodge(0.05))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.y = element_text(color="black",size=7),
        axis.text.x = element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  xlab(bquote('Total Phosphorus (µg ' *L^-1*')'))+ylab(" ")
ggsave("ALM rank plot.png",width=8, height=12, units="in", dpi=300)

# Figure: Rank plot of Iowa water bodies by total phosphorus concentrations - x-axix on log scale
alm_join %>%
  arrange(lakeName) %>%
  mutate(lakeName = factor(lakeName, levels=c("West Okoboji Lake", "Rudd Lake", "Arrowhead Lake", "Nine Eagles Lake", "Slip Bluff Lake",
                                              "Little Sioux Park Lake", "Mormon Trail Lake", "Kent Park Lake", "Willow Lake", "Lake Wapello",
                                              "Poll Miller Park Lake", "Lost Grove Lake", "Ada Hayden", "Pleasant Creek Lake", "Central Park Lake",
                                              "Green Castle Lake","Easter Lake","Yellow Smoke Park Lake","Lacey Keosauqua Park Lake","Lake MacBride",
                                              "Green Belt Lake","Cold Springs Lake","Nelson Park Lake","Lost Island Lake","Moorhead Park Pond",
                                              "Lake Cornelia","Lake Anita","Viking Lake","Mitchell Lake","Dog Creek (Lake)","Hawthorne Lake",
                                              "Springbrook Lake","Mariposa Lake","Belva Deer Lake","Nodaway Lake","Rathbun Reservoir",
                                              "Red Haw Lake","Crawford Creek Impoundment","Brushy Creek Lake","Three Mile Lake","Clear Lake",
                                              "Lake Sugema","Lake Ahquabi","Fogle Lake S.W.A.","Center Lake","George Wyth Lake",
                                              "East Okoboji Lake","Lake Meyer","Minnewashta Lake","Lake Icaria","Otter Creek Lake",
                                              "Briggs Woods Lake","Upper Gar Lake","Mill Creek Lake","Hickory Grove Lake","Hooper Area Pond",
                                              "Lower Gar Lake","Twelve Mile Creek Lake","Big Spirit Lake","Little River Watershed Lake","Big Creek Lake",
                                              "Eldred Sherwood Lake","Prairie Rose Lake","West Lake (Osceola)","Diamond Lake","Frog Hollow",
                                              "Greenfield Lake","Lake Manawa","Iowa Lake","North Twin Lake","Oldham Lake",
                                              "Lake of the Hills","Windmill Lake","Carter Lake","Upper Pine Lake","Wilson Park Lake",
                                              "Spring Lake","Five Island Lake","Lake of Three Fires","Lake Hendricks","South Prairie Lake",
                                              "Lake Keomah","Little Wall Lake","Badger Lake","Meadow Lake","Thayer Lake",
                                              "Roberts Creek Lake","Lower Pine Lake","Geode Lake","Arbor Lake","Arrowhead Pond",
                                              "Casey Lake","Beeds Lake","Blue Lake","Don Williams Lake","Crystal Lake",
                                              "Beaver Lake","Storm Lake (incl Little Storm Lake)","Lake Miami","Indian Lake","Big Hollow Lake",
                                              "Snyder Bend Lake","Hannen Lake","Black Hawk Lake","Rock Creek Lake","Silver Lake (Dickinson)","Silver Lake (Delaware)",
                                              "White Oak Conservation Area Lake","Ingham Lake","Meyer Lake","Lake Smith","Rodgers Park Lake",
                                              "Williamson Pond","Union Grove Lake","Lake Pahoja","Badger Creek Lake","Lake Darling",
                                              "DeSoto Bend","Green Valley Lake","Browns Lake","Ottumwa Lagoon","Silver Lake (Palo Alto)","Pierce Creek Pond",
                                              "Swan Lake","Manteno Park Pond","Littlefield Lake","Summit Lake","Avenue of the Saints Pond",
                                              "East Lake (Osceola)","Little Spirit Lake","Bob White Lake","Orient Lake"))) %>%
  ggplot(aes(x=meanTP_ugL,y=lakeName)) +
  annotate("rect", xmin = 12, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#1d91c0", alpha = 0.7)+
  annotate("rect", xmin = 24, xmax = 96, ymin = -Inf, ymax = Inf, fill = "#7fcdbb", alpha = 0.8)+
  annotate("rect", xmin = 96, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#c7e9b4", alpha = 0.8)+
  geom_point(size=1.8)+
  geom_errorbar(aes(y=lakeName, xmin=meanTP_ugL-semTP_ugL, xmax=meanTP_ugL+semTP_ugL),width=0,position=position_dodge(0.05))+
  scale_x_continuous(trans = 'log10')+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.y = element_text(color="black",size=7),
        axis.text.x = element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))+
  xlab(bquote('Total Phosphorus (µg ' *L^-1*')'))+ylab(" ")
ggsave("ALM rank plot log scale.png",width=8, height=12, units="in", dpi=300)





# Part 4 - Visualize spatial patterns in total phosphorus concentrations in Iowa lakes and reservoirs (IDNR, ALM) -----------------------------------------
# Read in shapefile for Iowa map
iowa<-st_read("county.shp") # Iowa county map

# Convert ALM file to spatial file
alm_full$pecentSRP<-(alm_full$meanSRP_ugL/alm_full$meanTP_ugL)*100 # first calculate soluble reactive P (SRP) as a percent of total P (TP)
sites<-st_as_sf(alm_full,coords=c("longitude","latitude"),crs=4893,agr="constant") #convert dataframe with AML sites to a spatial file (4893 is the CRS code for NAD83 - what Iowa county outline shp projected in)
names(sites)

# Figure: Maps of total phosphorus and soluble reactive phosphorus percent across the state of Iowa 
TP_map<-
ggplot() + 
  geom_sf(data = iowa, size = 0.8, color = "black", fill = "white") + 
  geom_sf(data=sites,aes(size=meanTP_ugL),color="darkorange3")+scale_size(range=c(1,7))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position="top")
SRPper_map<-
ggplot() + 
  geom_sf(data = iowa, size = 0.8, color = "black", fill = "white") + #f2f8f9 #e0eff1
  geom_sf(data=sites,aes(size=pecentSRP),color="darkorange")+scale_size(range=c(1,7))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position="top")
SRP_map<-
ggplot() + 
  geom_sf(data = iowa, size = 0.8, color = "black", fill = "white") + #f2f8f9 #e0eff1
  geom_sf(data=sites,aes(size=meanSRP_ugL),color="darkorange")+scale_size(range=c(1,7))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position="top")

maps<-grid.arrange(TP_map, SRP_map, nrow=1)
maps_per<-grid.arrange(TP_map, SRPper_map, nrow=1)
ggsave("TP SRP Map.png",maps, width=9, height=8, units="in", dpi=300)
ggsave("TP SRP per Map.png", maps_per, width=9, height=8, units="in", dpi=300)

#SRP concentration
ggplot() + 
  geom_sf(data = iowa, size = 0.8, color = "black", fill = "white") + #f2f8f9 #e0eff1
  geom_sf(data=sites,aes(size=meanSRP_ugL),color="darkorange")+scale_size(range=c(1,7))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position="top")


### Playing around with ALM data for stoich project
View(alm_join) #big spirit site ID is 22300014

bs<-subset(alm, siteID=="22300014")
ggplot(data=bs, aes(x=year, y=NOx, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

bh<-subset(alm, siteID=="22810002") #black hawk lake
ggplot(data=bh, aes(x=year, y=TP, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

blue<-subset(alm, siteID=="22670002") #blue lake
ggplot(data=blue, aes(x=year, y=TP, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

bc<-subset(alm, siteID=="22940002") #brushy creek
ggplot(data=bc, aes(x=year, y=tss, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

cl<-subset(alm, siteID=="22170001") #clear lake
ggplot(data=cl, aes(x=year, y=TN_calc, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

crystal<-subset(alm, siteID=="22410001") #crystal lake
ggplot(data=crystal, aes(x=year, y=TP, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

gw<-subset(alm, siteID=="22070001") #george wyth lake
ggplot(data=gw, aes(x=year, y=tss, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

lost<-subset(alm, siteID=="22740002") # lost island lake
ggplot(data=lost, aes(x=year, y=TP, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")

silver_de<-subset(alm, siteID=="22280001") # silver (delaware co)
ggplot(data=silver_de, aes(x=year, y=SRP, color=doy))+
  geom_point()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Year")+ylab("...")
