#-------------------------------------------------------------------------------
# Script: enterprise_prep.R
# Author: Nolan Young Zabala
# Description: - load 2013 data for Bangladesh
#              - select relevant variables
#              - output intermediate data files for analysis
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(haven)
library(dplyr)
library(sf)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------

# --- PERFORM ONCE AND NEVER AGAIN - SUBSET COMPREHENSIVE
# Comprehensive dataset
# master <- read_dta("raw_data/master_analysis.dta")

# bangla13 <- master %>% 
#  filter(cy == 11) # Filter country var from comprehensive dataset

#write.csv(bangla13, "raw_data/Bangla_13.csv")

# Remove "master" from memory - don't need anymore and too big
# rm(master)
# ---

# Load Bangladesh Enterprise Survey 2013
bangla13 <- read.csv("raw_data/Bangla_13.csv")

# Load shapefile for adding coordinates
point <- read_sf("raw_data/shapefiles/firm_pointfile.shp")

# Load SPEI file
spei <- read.csv("raw_data/wld_cli_terraclimate_spei12_1959_2021_point.csv")

#------------------------ 3. ADD COORDINATES + SPEI ----------------------------

# Merge with shapefile
bangla13 <- left_join(bangla13, point, by = "idstd")

# Remove shapefile to save memory
rm(point)

# Add "spei_" to the start of each column name in the SPEI dataframe
# to more easily identify the variable once merged to the bangla13 dataframe.
new_colnames <- paste("spei", colnames(spei), sep = "_")
colnames(spei) <- new_colnames

# Rename firm identifier in SPEI dataframe
spei <- spei %>% 
  rename("idstd" = "spei_firm_point")

# Merge SPEI with bangla13
bangla13 <- left_join(bangla13, spei, by = "idstd")

# Remove spei to save memory
rm(spei)

#-------------------------- 4. SELECT RELEVANT VARS ----------------------------

# Create list of relevant vars I want to use for analysis
# Divide them up by type for ease of reference

# Firm vars
firm_location_vars <- c("idstd", "wt", "wt_rs", "lat_mask", "lon_mask", "a2x", "a3ax")
firm_size_vars <- c("size", "a7", "b6")
firm_sector_vars <- c("stra_sector", "isic")
firm_gender_vars <- c("b4", "b4a")

# Climate vars

# The vars are repeated across many columns for different years... So this is
# a function which will create a regular expression to use when selecting
# multiple columns starting with the same string, e.g. "hotdays"
prefix <- function(prefix_list){
  paste("^", paste(prefix_list, collapse = "|"), sep = "")
    }

climate_temp_vars <- c("hotdays_mon_sum")
climate_temp_pattern <- prefix(climate_temp_vars)

climate_rain_vars <- c("cwd_5mm_mon_max", "precip_mon")
climate_rain_pattern <- prefix(climate_rain_vars)

climate_drought_vars <- c("cdd_1mm_mon", "drydays_5mm_mon", "spei")
climate_survey_drought_vars <- c("c15", "c16")
climate_drought_pattern <- prefix(climate_drought_vars)

bangla13_subset <- bangla13 %>% 
  select(firm_location_vars, firm_size_vars, firm_sector_vars,
         firm_gender_vars, matches(climate_temp_pattern), 
         matches(climate_rain_pattern), climate_survey_drought_vars,
         matches(climate_drought_pattern))


#---------------------- 5. WRITE INTERMEDIATE DATA FILES -----------------------

# Write bangla13_subset to csv
#write.csv(bangla13_subset, "intermediate_data/Bangla13_FirmAndClimate.csv")

# Subset to just firm vars and write to csv
bangla13_just_firm <- bangla13_subset %>% 
  select(firm_location_vars, firm_size_vars, firm_sector_vars,
         firm_gender_vars)

#write.csv(bangla13_just_firm, "intermediate_data/Bangla13_Firm.csv")

