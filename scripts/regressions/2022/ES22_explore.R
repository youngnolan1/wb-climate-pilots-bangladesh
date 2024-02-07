#-------------------------------------------------------------------------------
# Script: ES22_explore.R
# Author: Nolan Young Zabala
# Description: - explore 2022 ES data
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(haven)
library(sf)
library(leaflet)
library(ggplot2)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------

# ES 2022 data
bangla22 <- read_dta("raw_data/Bangladesh-2022-full-data.dta") %>% 
  dplyr::mutate(region = as.character(haven::as_factor(a3a)))

# Shapefile
shapefile <- read_sf("raw_data/shapefiles/gadm41_BGD_1.shp") %>% 
  rename(region = NAME_1) %>% 
  dplyr::select(region, geometry)


#--------------------------- 3. MERGE WITH SHAPEFILE ---------------------------

# Chittagong
bangla22$region[bangla22$region == "Chattogram"] <- "Chittagong"
bangla22$region[bangla22$region == "Cox's Bazar"] <- "Chittagong"

# Dhaka
bangla22$region[bangla22$region == "Dhaka MA"] <- "Dhaka"
bangla22$region[bangla22$region == "Greater Dhaka"] <- "Dhaka"

# Barisal
bangla22$region[bangla22$region == "Barishal"] <- "Barisal"

# Merge
merged <- left_join(shapefile, bangla22, by = "region")


#--------------------------- 4. PLOT NUMBER OF FIRMS ---------------------------

# Heat map function
heatplot <- function(df, plot_title, legend_title){   
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient(low = "lightgrey", high = "red") +     
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}

# Group by grid and count firms
sf_firm_count <- merged %>% 
  mutate(count = 1) %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(count, na.rm = TRUE)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))

heatplot(sf_firm_count, "Bangladesh ES 2022 - Firms", "Count")


#------------------------- 5. MERGE WITH CLIMATE DATA --------------------------

# CLIMATE DATA: regional averages of 2000-21 avg temps per firm

# Load bangla13 climate data
bangla13 <- read.csv("intermediate_data/BanglaRegAnalysisData.csv")

# Drop firm vars
bangla13 <- bangla13 %>% 
  dplyr::select(lat_mask, lon_mask, avg_temp, total_hotdays, temp_volatility, avg_spei)

# Merge with gadm1
bangla13 <- st_as_sf(bangla13, 
                     coords = c("lon_mask", "lat_mask"), 
                     crs = st_crs(shapefile)) 

# Assign firms to grids
bangla13 <- st_join(shapefile,
                     bangla13,
                     left = TRUE)

# Drop geometry
bangla13 <- bangla13 %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  filter(region != "Barisal")

# Take averages by region
bangla13_climate_avgs <- bangla13 %>% 
  group_by(region) %>% 
  summarise(temp = mean(avg_temp, na.rm = TRUE),
            tempvolatility = mean(temp_volatility, na.rm = TRUE),
            hotdays = mean(total_hotdays, na.rm = TRUE),
            spei = mean(avg_spei, na.rm = TRUE))

# Merge with bangla22
reg_master <- left_join(bangla22, bangla13_climate_avgs, by = "region")

# Remove rest
rm(bangla13, bangla13_climate_avgs, bangla22, merged, shapefile)

#---------------------------- 6. PREPARE REG DATA ------------------------------

# Create age var
reg_master$firm_age <- 2022 - reg_master$b5

# Create size var
reg_master <- reg_master %>% 
  mutate(size = case_when(
    stratificationsizecode == 1 ~ 1,
    stratificationsizecode == 2 ~ 2,
    stratificationsizecode == 3 ~ 3,
    stratificationsizecode == 4 ~ 3,
    stratificationsizecode == 5 ~ 3
  ))

# Select relevant vars for reg models and rename
regvars <- c("wstrict", "k4", "n5a", "n5b", "d2", "l1", "temp", "hotdays", 
             "spei", "tempvolatility", "firm_age", "size", "region", "b3", 
             "b7", "b7a", "e1", "c30a", "d30a", "d30b", "j30c", "k30", "l30a", 
             "c15", "d3a", "d3b", "d3c", "k6", "k7", "k8")

reg_master <- reg_master %>%
  dplyr::select(all_of(regvars)) %>%
  rename(invest_fixed_asset_dummy = k4) %>% 
  rename(equipment_fixed_asset_exp = n5a) %>% 
  rename(buildings_fixed_asset_exp = n5b) %>% 
  rename(sales = d2) %>% 
  rename(workers = l1) %>% 
  rename(pcnt_owned_by_largest_owner = b3) %>%
  rename(manager_experience_yrs = b7) %>%
  rename(top_manager_female = b7a) %>%
  rename(market_served = e1) %>%
  rename(obstacle_electricity = c30a) %>%
  rename(obstacle_transport = d30a) %>%
  rename(obstacle_traderegulations = d30b) %>%
  rename(obstacle_businesslicensing = j30c) %>%
  rename(obstacle_accesstofinance = k30) %>%
  rename(obstacle_laborregulations = l30a) %>% 
  rename(insufficient_water_supply = c15) %>% 
  rename(pcnt_sales_national = d3a) %>% 
  rename(pcnt_sales_indirect_exports = d3b) %>% 
  rename(pcnt_sales_direct_exports = d3c) %>% 
  rename(checking_savings_account = k6) %>% 
  rename(overdraft_facility = k7) %>% 
  rename(financial_inst_credit = k8)

# Define NAs properly
reg_master[reg_master == -9] <- NA
reg_master[reg_master == -7] <- NA

# Define investment in fixed assets outcome var properly
# Change NAs to zeroes for those where dummy shows there was no investment
reg_master$equipment_fixed_asset_exp[reg_master$invest_fixed_asset_dummy == 2] <- 0
reg_master$buildings_fixed_asset_exp[reg_master$invest_fixed_asset_dummy == 2] <- 0

# Change NAs to zeroes for those who have expenditure in one column but not the other
reg_master$equipment_fixed_asset_exp[is.na(reg_master$buildings_fixed_asset_exp) == FALSE] <- 0
reg_master$buildings_fixed_asset_exp[is.na(reg_master$equipment_fixed_asset_exp) == FALSE] <- 0

# Sum the expenditures to get a total investment outcome var
reg_master$total_invest_fixed_asset <- reg_master$equipment_fixed_asset_exp + reg_master$buildings_fixed_asset_exp

# Define sales per worker outcome var
reg_master$sales_per_worker <- reg_master$sales / reg_master$workers

# Deal with categorical vars
obstacle_mutate <- function(var){
  reg_master <- reg_master %>%
    mutate(!!var := case_when(
      !!sym(var) == 0 ~ "No",
      !!sym(var) == 1 ~ "Minor",
      !!sym(var) == 2 ~ "Moderate",
      !!sym(var) == 3 ~ "Major",
      !!sym(var) == 4 ~ "VerySevere"
    ) %>%
      as.factor() %>%
      factor(levels = c("No", "Minor", "Moderate", "Major", "VerySevere"))
    )
}

obstacle_vars <- c("obstacle_electricity", "obstacle_transport", "obstacle_traderegulations",
                   "obstacle_businesslicensing", "obstacle_accesstofinance", "obstacle_laborregulations")

for (var in obstacle_vars){
  reg_master <- obstacle_mutate(var)
}

reg_master <- reg_master %>%
  mutate(size = case_when(
    size == 1 ~ "Small",
    size == 2 ~ "Medium",
    size == 3 ~ "Large"
  ))
reg_master$size <- as.factor(reg_master$size)

reg_master$region <- as.factor(reg_master$region)

reg_master <- reg_master %>%
  mutate(market_served = case_when(
    market_served == 1 ~ "Local",
    market_served == 2 ~ "National",
    market_served == 3 ~ "International"
  ))
reg_master$market_served <- as.factor(reg_master$market_served)

# Re-code the binary vars to 0/1
reg_master <- reg_master %>%
  mutate(top_manager_female = case_when(
    top_manager_female == 1 ~ 1, # Yes, top manager is female
    top_manager_female == 2 ~ 0  # No, not female
  )) %>% 
  mutate(insufficient_water_supply = case_when(
    insufficient_water_supply == 1 ~ 1, 
    insufficient_water_supply == 2 ~ 0  
  )) %>% 
  mutate(checking_savings_account = case_when(
    checking_savings_account == 1 ~ 1, 
    checking_savings_account == 2 ~ 0  
  )) %>% 
  mutate(overdraft_facility = case_when(
    overdraft_facility == 1 ~ 1, 
    overdraft_facility == 2 ~ 0  
  )) %>% 
  mutate(financial_inst_credit = case_when(
    financial_inst_credit == 1 ~ 1,
    financial_inst_credit == 2 ~ 0
  ))

# Create pcnt sales national vs exports dummy
reg_master$pcnt_sales_exports <- reg_master$pcnt_sales_direct_exports + reg_master$pcnt_sales_indirect_exports
reg_master$majority_national_sales <- ifelse(reg_master$pcnt_sales_national > reg_master$pcnt_sales_exports, 1, 0)

# Standardize model inputs (but not categorical)
standard_vars <- c("firm_age", "pcnt_owned_by_largest_owner", "manager_experience_yrs")
reg_master[standard_vars] <- lapply(reg_master[standard_vars], function(x) x / sd(x, na.rm = TRUE))

# Write csv
#write.csv(reg_master, "intermediate_data/BanglaRegAnalysisData22.csv")

