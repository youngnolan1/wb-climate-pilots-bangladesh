#-------------------------------------------------------------------------------
# Script: SEZ_data_clean.R
# Author: Nolan Young Zabala
# Description: - explore SEZ data
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(readxl)
library(leaflet)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------

sez <- read_excel("raw_data/ES Data_18Jun21_review_coords.xlsx")


#-------------------------------- 3. SUBSET DATA -------------------------------

sez <- sez %>% 
  filter(Country == "Bangladesh") %>% 
  select(`UNIQUE Case ID`, Longitude, Latitude) %>% 
  rename(id = `UNIQUE Case ID`) %>% 
  mutate(id = 1)

# Read Bangladesh shapefile, select polygon data, and add grid_id
country_and_subdivisions <- read_sf("raw_data/shapefiles/gadm41_BGD_2.shp") %>%    
  select(geometry) %>% 
  mutate(grid_id = row_number())

# Convert sez dataframe to sf object
sf_sez <- st_as_sf(sez, 
                   coords = c("Longitude", "Latitude"), 
                   crs = st_crs(country_and_subdivisions)) 

# Assign firms to grids
sez_joined <- st_join(country_and_subdivisions,
                     sf_sez,
                     left = TRUE)

# Group by grid and count
sez_firm_count <- sez_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(id))

sez_firm_count$HeatVar <- replace(sez_firm_count$HeatVar, is.na(sez_firm_count$HeatVar), 0)


#write.csv(sez, "Bangla_SEZ_FirmLocation.csv")


#----------------- 4. COMBINE WITH ES FOR MAPPING TOGETHER ---------------------

enterprise <- read.csv("intermediate_data/Bangla13_Firm.csv") %>% 
  select(idstd, lat_mask, lon_mask) %>% 
  rename(id = idstd) %>% 
  mutate(id = 0) %>% 
  rename(Latitude = lat_mask) %>% 
  rename(Longitude = lon_mask)

firm_combo <- rbind(sez, enterprise)
