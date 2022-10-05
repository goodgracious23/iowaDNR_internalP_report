# Data from Ambient Lakes Monitoring Program - historical data 2000-2021
# Includes 2008 SHL data in addition to ISU long-term program
# Downloaded from AQUiA database in Sept 2021

# Libraries
if (!require(tidyverse)) install.packages('tidyverse')
library('tidyverse')
if (!require(lubridate)) install.packages('lubridate')
library('lubridate')

# Read in the data
wide_alm <- read_csv('Iowa Lakes Dataset 2000-2022.csv') %>%
#Make the dates something we can use
mutate(date = parse_date_time(sampleDate, orders = "mdy HMS"),
       date = as.POSIXct(date),
       doy = yday(date),
       year = year(date)) %>%
  #Remove extraneous columns
  select(-facilityID, - aquifer, -sampleDate, -projectCode, -cas_rn, 
         -fraction, -relDepth, -unit:-remark) %>%
  #Pivot from long to wide format by sampling date
  pivot_wider(id_cols = c(year, doy, siteID), 
              names_from = analyte, 
              values_from = result,
              values_fn = {mean}) %>%
  # The analyte column is messy based on weird capitalization errors (e.g., AS P vs as P)
  # Let's do some combining of columns to fix this unholy mess
  
  #Total Phosphorus
  mutate(TP = case_when(is.na(.$`Phosphate-phosphorus (as P)`) ~ `Phosphate-phosphorus (AS P)`, 
                      TRUE ~ `Phosphate-phosphorus (as P)`)) %>%
  select(-`Phosphate-phosphorus (as P)`, -`Phosphate-phosphorus (AS P)`) %>%
  mutate(TP = case_when(is.na(.$`TP`) ~ `Phosphorus (as P)`, TRUE ~ `TP`)) %>%
  select(-`Phosphorus (as P)`) %>%
  #Orthophosphate
  mutate(SRP = case_when(is.na(.$`Orthophosphate (as P)`) ~ `Orthophosphate (AS P)`, 
                         TRUE ~ `Orthophosphate (as P)`)) %>%
  select(-`Orthophosphate (as P)`, -`Orthophosphate (AS P)`) %>%
  # Calcium Carbonate
  mutate(CaCO3 = case_when(is.na(.$`Calcium carbonate (as CaCO3)`) ~ 
                             `Calcium carbonate (AS CACO3)`, 
                         TRUE ~ `Calcium carbonate (as CaCO3)`)) %>%
  select(-`Calcium carbonate (as CaCO3)`, -`Calcium carbonate (AS CACO3)`) %>%
  # Chlorophyll
  mutate(chlorophyll = case_when(is.na(.$`Chlorophyll a`) ~ `Chlorophyll a, free of pheophytin`, 
                           TRUE ~ `Chlorophyll a`)) %>%
  select(-`Chlorophyll a`, -`Chlorophyll a, free of pheophytin`) %>%
  # Nitrate + Nitrite
  mutate(NOx = case_when(is.na(.$`Inorganic nitrogen (nitrate and nitrite) (as N)`) ~ 
                           `Inorganic nitrogen (nitrate and nitrite) (AS N)`, 
                                 TRUE ~ `Inorganic nitrogen (nitrate and nitrite) (as N)`)) %>%
  select(-`Inorganic nitrogen (nitrate and nitrite) (as N)`, 
         -`Inorganic nitrogen (nitrate and nitrite) (AS N)`) %>%
  #Ammonium + Ammonia
  mutate(NHx = case_when(is.na(.$`Ammonia-nitrogen (as N)`) ~ `Ammonia-nitrogen (AS N)`, 
                                 TRUE ~ `Ammonia-nitrogen (as N)`)) %>%
  select(-`Ammonia-nitrogen (as N)`, -`Ammonia-nitrogen (AS N)`) %>%
  
  #Let's clean up everything else
  #delete the unionized ammonia columns
  select(-Ammonia, -`Ammonia (as NH3)`, -`Ammonia (as N)`, -`Ammonia (AS N)`) %>%
  #rename column headers to things we want to type
  rename(maxDepth = `Depth, bottom`,
         spCond = `Specific conductance`,
         epiTemp = `Temperature, water`,
         epiTurbidity = Turbidity,
         alkalinity = `Alkalinity, total`,
         TN_measured = `Nutrient-nitrogen`,
         iss = `Fixed suspended solids`,
         tss = `Total suspended solids`,
         vss = `Volatile Suspended Solids`,
         secchiDepth = `Depth, Secchi disk depth`,
         epiDO_mgL = `Dissolved oxygen (DO)`,
         epiDO_sat = `Dissolved oxygen saturation`,
         DOC = `Carbon`,
         epi_pH = pH,
         Ecoli = `Escherichia coli`,
         TKN = `Kjeldahl nitrogen (AS N)`,
         epiTDS = `Total dissolved solids`,
         thermoclineDepth = `Depth, thermocline`,
         windSpeed = `Wind velocity`) %>%
  #Calculate total nitrogen from Kjeldahl nitrogen and NOx
  mutate(TN_calc = TKN + NOx) %>%
  #Remove unuseful variables
  select(-`Precipitation during activity (choice list)`,
         -`Temperature, air`,
         -`Wind direction (direction from, expressed 0-360 deg)`,
         -`Wind force, Beaufort scale`,
         -`Total Coliform`,
         -`Barometric pressure`,
         -Microcystin, -Phycocyanin, -Ecoli, -windSpeed)

write.csv(wide_alm, file = "ALM Data 2000-2021 WIDE.csv")
