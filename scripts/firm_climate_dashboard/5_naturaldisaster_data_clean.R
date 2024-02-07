#-------------------------------------------------------------------------------
# Script: 5_naturaldisaster_data_clean.R
# Author: Nolan Young Zabala
# Description: 
#-------------------------------------------------------------------------------


# --------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(dplyr)
library(sf)
library(readxl)
library(haven)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")

# #-------------------------------- 2. LOAD DATA ---------------------------------
#
# # Load shapefiles
# districts <- read_sf("raw_data/shapefiles/gadm41_BGD_2.shp") %>%
#   select(geometry) %>%
#   mutate(grid_id = row_number())
# #
# # # Load disasters data
# disasters <- read_dta("intermediate_data/natural_disasters_4_countries_cleaned.dta")
# #
# # # Filter and subset
# disasters <- disasters %>%
#   filter(Bangladesh == 1) %>%
#   filter(eventtype != "EARTHQUAKES") %>%
#   select(EVENTID, lon, lat, eventtype, alertlevel, year_start, intens_days,
#          death, displaced) %>%
#   mutate(displaced = displaced/1000) # Displaced in 1000's
# 
# 
# #----------------------------- 3. ASSIGN TO GRIDS ------------------------------
# 
# # # Convert to sf object
# sf_disasters <- st_as_sf(disasters,
#                          coords = c("lon", "lat"),
#                          crs = st_crs(districts))
# 
# # # Assign to grids
# disasters_by_district <- st_join(districts,
#                      sf_disasters,
#                      left = TRUE)
# 
# # Write shapefile
# st_write(disasters_by_district, "intermediate_data/disasters_by_district.shp")


#------------------------ 4. METRICS GROUPED BY GRID ---------------------------

# Periods
#pre_period <- 2000:2010
#current_period <- 2011:2021
#entire_period <- 2000:2021

# FIRST TRY - ALL DISASTERS, NOT DIVIDING UP BY DECADE

disasters_by_district <- read_sf("intermediate_data/disasters_by_district.shp")

# Number of events
disasters_by_district$EVENTID[!is.na(disasters_by_district$EVENTID)] <- 1
disasters_by_district$EVENTID <- as.numeric(disasters_by_district$EVENTID)

sf_disaster_count <- disasters_by_district %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(EVENTID))


# Heat by alert level (green/orange/red)
sf_disaster_alert <- disasters_by_district %>% 
  mutate(alrtlvl = recode(alrtlvl, "GREEN" = 1, "ORANGE" = 2, "RED" = 3)) %>% 
  group_by(geometry) %>% 
  summarise(HeatVar = mean(alrtlvl))

heatplot_alert <- function(df, plot_title, legend_title){   
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient(low = "green", high = "red") +     
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}

# Total number of casualties - BIG OUTLIER, EXTREME EVENT
sf_disaster_casualties <- disasters_by_district %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(death))


# Total number of displaced people
sf_disaster_displaced <- disasters_by_district %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(displcd))

