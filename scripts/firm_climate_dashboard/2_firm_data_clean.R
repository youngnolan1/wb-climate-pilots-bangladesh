#-------------------------------------------------------------------------------
# Script: 2_firm_data_clean.R
# Author: Nolan Young Zabala
# Description: - set up Bangladesh grids
#              - assign firms to grids
#              - basic calculations and data preparation for firm maps (location,
#                size, sector)
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------
  
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(gridExtra)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------

# Read firm survey data
firms <- read.csv("intermediate_data/Bangla13_Firm.csv")

# Replace NAs
firms[firms == -99] <- NA
firms[firms == -9] <- NA
firms[firms == -7] <- NA

# Read Bangladesh shapefile, select polygon data, and add grid_id
country_and_subdivisions <- read_sf("raw_data/shapefiles/gadm41_BGD_2.shp") %>%    
  select(geometry) %>% 
  mutate(grid_id = row_number())

# Convert firm dataframe to sf object
sf_firms <- st_as_sf(firms, 
                     coords = c("lon_mask", "lat_mask"), 
                     crs = st_crs(country_and_subdivisions)) 

# Assign firms to grids
sf_joined <- st_join(country_and_subdivisions,
                     sf_firms,
                     left = TRUE)

# Save sf_joined to shapefile
#st_write(sf_joined, "intermediate_data/sf_joined.shp")


#------------------------------ 3. PLOT FUNCTION -------------------------------

# General function which creates a heatmap for the "HeatVar" of the given "df"
heatplot <- function(df, plot_title, legend_title){   
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient(low = "lightgrey", high = "red") +     
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}

# Group by grid and find weighted proportion
sf_firm_count <- sf_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))


#-------------------------------- 5. FIRM SIZE ---------------------------------

# 5a. Average "size" (1-3 classification) - weighted
  
# Group by grid and find average
sf_avgsize <- sf_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = weighted.mean(size, wt_rs, na.rm = TRUE))


# 5b. Average number of employees - weighted
  
# Group by grid and find average
sf_avgnumemployees <- sf_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = weighted.mean(b6, wt_rs, na.rm = TRUE))


#-------------------------------- 6. SECTORS ---------------------------------

# Assign standard isic sector classification (common across countries)
df_joined <- st_drop_geometry(sf_joined)

df_joined <- df_joined %>%
  mutate(isic_sector = case_when(
    between(isic, 1, 2) ~ "Agriculture, hunting and forestry",
    isic == 5 ~ "Fishing",
    between(isic, 10, 14) ~ "Mining and quarrying",
    between(isic, 15, 37) ~ "Manufacturing",
    between(isic, 40, 41) ~ "Electricity, gas and water supply",
    isic == 45 ~ "Construction",
    between(isic, 50, 52) ~ "Wholesale and retail trade",
    isic == 55 ~ "Hotels and restaurants",
    between(isic, 60, 64) ~ "Transport, storage and communications",
    between(isic, 65, 67) ~ "Financial intermediation",
    between(isic, 70, 74) ~ "Real estate, renting and business activities",
    isic == 80 ~ "Education",
    isic == 85 ~ "Health and social work",
    between(isic, 90, 93) ~ "Other community, social and personal service activities"
  ))



# 6a. Firm counts per sector - weighted proportions

sector_count <- function(sector){
  sector_sum <- df_joined %>% 
    select(idstd, grid_id, isic_sector, wt_rs) %>% 
    filter(isic_sector == sector) %>% 
    group_by(grid_id) %>% 
    summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
    mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
    mutate(HeatVar = case_when(
      HeatVar == 0 ~ NA,
      HeatVar != 0 ~ HeatVar
    ))
  
  sector_result <- left_join(country_and_subdivisions, sector_sum, by = "grid_id")

  return(sector_result)
}

