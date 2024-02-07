#-------------------------------------------------------------------------------
# Script: regression_analysis_dataprep.R
# Author: Nolan Young Zabala
# Description: - prepare reg_master data
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(fastDummies)
library(QuantPsyc)
library(matrixStats)

# # Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


# #-------------------------------- 2. LOAD DATA ---------------------------------

# THESE FILES ARE EQUIVALENT TO THE MASTER_ANALYSIS.DTA FILE (BUT JUST FOR
# BANGLADESH) AND THE SPEI FILE
master <- read.csv("raw_data/Bangla_13.csv")


# Load shapefile for adding coordinates
point <- read_sf("raw_data/shapefiles/firm_pointfile.shp")

# Merge with shapefile
master <- left_join(master, point, by = "idstd")

# Remove shapefile to save memory
rm(point)

# Load spei data
spei <- read.csv("raw_data/wld_cli_terraclimate_spei12_1959_2021_point.csv")

# #----------------- 3. CALCULATE CLIMATE AND AGE VARS ---------------------------
#
# # --- TEMPERATURE ---
master$avg_temp <- rowMeans(master[, c("mean_temp_m1", "mean_temp_m2", "mean_temp_m3", "mean_temp_m4", "mean_temp_m5", "mean_temp_m6", "mean_temp_m7", "mean_temp_m8", "mean_temp_m9", "mean_temp_m10", "mean_temp_m11", "mean_temp_m12")])

# # --- HOTDAYS ---
master$total_hotdays <- rowSums(master[, c("hotdays_m1", "hotdays_m2", "hotdays_m3", "hotdays_m4",
                                           "hotdays_m5", "hotdays_m6", "hotdays_m7", "hotdays_m8",
                                           "hotdays_m9", "hotdays_m10", "hotdays_m11", "hotdays_m12")])

# # --- TEMPERATURE VOLATILITY ---
master$temp_volatility <- rowSds(as.matrix(master[, c("mean_temp_m1", "mean_temp_m2", "mean_temp_m3", "mean_temp_m4", "mean_temp_m5", "mean_temp_m6", "mean_temp_m7", "mean_temp_m8", "mean_temp_m9", "mean_temp_m10", "mean_temp_m11", "mean_temp_m12")]), na.rm = TRUE)

# # --- PRECIP ---
# # Add "spei_" to the start of each column name in the SPEI dataframe
# # to more easily identify the variable once merged to the master dataframe.
new_colnames <- paste("spei", colnames(spei), sep = "_")
colnames(spei) <- new_colnames

# # Rename firm identifier in SPEI dataframe
spei <- spei %>%
  rename("idstd" = "spei_firm_point")

# # Merge with master
master <- left_join(master, spei, by = "idstd")

# # Calculate avg spei per month from 2000-2021
month_indices <- seq(1, 12)

# # Create empty dataframe to store monthly averages
spei_monthly_averages <- data.frame(matrix(NA, ncol = 12, nrow = nrow(master)))
colnames(spei_monthly_averages) <- paste("mean_spei_m", month_indices, sep = "")

# # Loop through each month and calculate the mean across all years
for (month in month_indices) {
  month_cols <- grep(paste0("spei_X20\\d{2}", sprintf("%02d", month), "01"), colnames(master))
  spei_monthly_averages[, paste("mean_spei_m", month, sep = "")] <- rowMeans(master[, month_cols, drop = FALSE], na.rm = TRUE)
}

spei_monthly_averages$avg_spei <- rowMeans(spei_monthly_averages[, c("mean_spei_m1", "mean_spei_m2", "mean_spei_m3", "mean_spei_m4", "mean_spei_m5", "mean_spei_m6", "mean_spei_m7", "mean_spei_m8", "mean_spei_m9", "mean_spei_m10", "mean_spei_m11", "mean_spei_m12")])

# Drop spei columns to save memory
master <- master[, -grep("^spei", colnames(master))]

# Add SPEI mean to precip_master
master$avg_spei <- spei_monthly_averages$avg_spei

# --- AGE ---
master$firm_age <- 2013 - master$b5

# #------------------------- 4. SUBSET TO REG VARS -------------------------------
#
regvars <- c("idstd", "lat_mask", "lon_mask", "wt", "log_rev", "k4", "n5a", "n5b", "d2", "l1", "avg_temp", "total_hotdays", 
             "avg_spei", "temp_volatility", "firm_age", "size", "a2x", "b3", 
             "b7", "b7a", "e1", "c30a", "d30a", "d30b", "j30c", "k30", "l30a", 
             "c15", "d3a", "d3b", "d3c", "j4", "k6", "k7", "k8")

reg_master <- master %>%
  dplyr::select(all_of(regvars)) %>%
  rename(invest_fixed_asset_dummy = k4) %>% 
  rename(equipment_fixed_asset_exp = n5a) %>% 
  rename(buildings_fixed_asset_exp = n5b) %>% 
  rename(sales = d2) %>% 
  rename(workers = l1) %>% 
  rename(sampling_region = a2x) %>%
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
  rename(freq_meetings_tax_officials = j4) %>% 
  rename(checking_savings_account = k6) %>% 
  rename(overdraft_facility = k7) %>% 
  rename(financial_inst_credit = k8)
  
  

# # Define NAs properly
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

# # Deal with categorical vars
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


reg_master$sampling_region <- as.factor(reg_master$sampling_region)

reg_master <- reg_master %>%
  mutate(market_served = case_when(
    market_served == 1 ~ "Local",
    market_served == 2 ~ "National",
    market_served == 3 ~ "International"
  ))
reg_master$market_served <- as.factor(reg_master$market_served)

#
# # Re-code the binary vars to 0/1
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



#------------------------- 5. EXPORT -----------------------------------

#
# # Standardize model inputs (but not categorical)
standard_vars <- c("firm_age", "pcnt_owned_by_largest_owner", "manager_experience_yrs",
                   "freq_meetings_tax_officials")

reg_master[standard_vars] <- lapply(reg_master[standard_vars], function(x) x / sd(x, na.rm = TRUE))

# WRITE CSV FILE
write.csv(reg_master, "intermediate_data/BanglaRegAnalysisData.csv")
