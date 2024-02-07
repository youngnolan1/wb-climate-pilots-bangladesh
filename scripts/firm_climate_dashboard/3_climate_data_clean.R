#-------------------------------------------------------------------------------
# Script: climate_data_clean.R
# Author: Nolan Young Zabala
# Description: - format climate data in usable way
#              - compute metrics like old averages/changes/etc
#              - output data for use in dashboard
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(sf)
library(matrixStats)
library(leaflet)
library(ggplot2)
library(gridExtra)
library(robustHD)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------

# Read sf_joined
sf_joined <- read_sf("intermediate_data/sf_joined.shp")

# Read climate data
climate <- read.csv("intermediate_data/Bangla13_FirmAndClimate.csv")

# Save weights
weights <- climate %>% 
  dplyr::select(idstd, wt_rs)

# Get rid of vars I don't need
not_needed <- c("X", "a2x", "a3ax", "size", "a7", "b6", "stra_sector", "b4", "b4a")

climate <- climate %>% 
  dplyr::select(-all_of(not_needed))

# Replace NAs
climate[climate == -99] <- NA
climate[climate == -9] <- NA
climate[climate == -7] <- NA


#------------------------ 3. PRE/CURRENT/ENTIRE PERIOD -------------------------

# Pre-period
pre_period <- 2000:2010

# Current period
current_period <- 2011:2021

# Entire period
entire_period <- 2000:2021


#---------------------------- 4. GENERAL FUNCTIONS -----------------------------


# METHOD: take yearly total e.g. hot days; find weighted average of 2000-10; find weighted average of
# 2011-21; compute change


# Function which subsets for given climate var
climate_var_subset <- function(climate_var){
  
  relevant_cols <- c("idstd", grep(climate_var, names(climate), value = TRUE))
  
  climate_var_df <- climate[, relevant_cols, drop = FALSE]
  
  return(climate_var_df)
}


# Function which computes yearly sum for given year
calculate_yearly_total <- function(climate_var_df, year) {

  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Sum all columns except idstd
  result <- varyear_df %>%
    mutate(!!year := rowSums(dplyr::select(., -idstd), na.rm = TRUE)) %>% 
    dplyr::select(idstd, !!year)
    
  return(result)
}

# Function which identifies yearly max for given year
calculate_yearly_max <- function(climate_var_df, year) {
  
  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Find max of all columns except idstd
  matrix_data <- varyear_df[,-1]
  row_max_values <- rowMaxs(as.matrix(matrix_data), na.rm = TRUE)
  
  # Add max to df
  varyear_df[[year]] <- row_max_values
  
  # Drop the monthly columns
  result <- varyear_df %>% 
    dplyr::select(idstd, !!year)
  
  return(result)
}

# Function which calculates yearly standard deviation
calculate_yearly_sd <- function(climate_var_df, year) {
  
  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Find stdev of all columns except idstd
  matrix_data <- varyear_df[,-1]
  row_stdev_values <- rowSds(as.matrix(matrix_data), na.rm = TRUE)
  
  # Add stdev to df
  varyear_df[[year]] <- row_stdev_values
  
  # Drop the monthly columns
  result <- varyear_df %>% 
    dplyr::select(idstd, !!year)
  
  return(result)
}


# Period means function
period_means <- function(df){
  
  result <- df %>%
    mutate(pre_mean = rowMeans(dplyr::select(., as.character(pre_period)), na.rm = TRUE),
           current_mean = rowMeans(dplyr::select(., as.character(current_period)), na.rm = TRUE)) %>%
    dplyr::select(idstd, pre_mean, current_mean)  
}


# Calculate percent change
percent_change <- function(df){
  result <- df %>%
    mutate(percent_change = ((current_mean - pre_mean) / pre_mean) * 100)
  
  return(result)
} 

# Calculate difference
difference <- function(df){
  result <- df %>% 
    mutate(difference = current_mean - pre_mean)
}


# Putting it all together - yearly totals or max, avg by decade, difference
yearlymetric_decadeavg_diff <- function(variable, metric){
  
  # Subset to only climate var
  var_df <- climate_var_subset(variable)
  
  
  # Calculate yearly totals for each year
  wrapper <- function(year) {
    metric(climate_var_df = var_df, year)
  }
  
  var_totals <- lapply(entire_period, wrapper)
  
  var_totals <- Reduce(function(x, y) merge(x, y, by = "idstd", all = TRUE), var_totals)
  
  
  # Calculate period means and difference
  var_means <- period_means(var_totals)
  
  var_means <- difference(var_means)
  
  
  # Merge with sf_joined
  sf_var <- left_join(sf_joined, var_means, by = "idstd")
  
  # Find average difference per grid
  sf_var <- sf_var %>% 
    group_by(geometry) %>% 
    summarize(HeatVar = weighted.mean(difference, wt_rs, na.rm = TRUE))
}


# Climate-var-specific heatplot function (deals with negative values)
heatplot_climate <- function(df, plot_title, legend_title){
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}


#------------------------------- 5. TEMPERATURE --------------------------------

#Hotdays
sf_hotdays <- yearlymetric_decadeavg_diff("hotdays", calculate_yearly_total)

#Hotdays volatility
sf_hotdays_volatility <- yearlymetric_decadeavg_diff("hotdays", calculate_yearly_sd)

#spei
sf_spei <- yearlymetric_decadeavg_diff("spei", calculate_yearly_total)


#-------------------------------- 6. RAINFALL ----------------------------------

#Cwd
sf_cwd <- yearlymetric_decadeavg_diff("cwd", calculate_yearly_max)

#Precip - yearly total
sf_precip <- yearlymetric_decadeavg_diff("precip", calculate_yearly_total)

#Precip volatility
sf_precip_volatility <- yearlymetric_decadeavg_diff("precip", calculate_yearly_sd)

#sf_precip <- yearly_extreme_decade_diff_compute(WHICH VAR?)


#------------------------------- 7. DROUGHT ------------------------------------

#Drydays
sf_drydays <- yearlymetric_decadeavg_diff("drydays", calculate_yearly_total)

#Cdd
sf_cdd <- yearlymetric_decadeavg_diff("cdd", calculate_yearly_max)

#Water availability an issue (c15)
sf_water_availability <- climate_var_subset("c15") %>% 
  filter(c15 == 1) 

sf_water_availability <- left_join(sf_joined, sf_water_availability, by = "idstd") %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(c15, na.rm = TRUE)) %>% 
  mutate(HeatVar = replace(HeatVar, HeatVar == 0, NA))


# #--------- WINSORIZING EXERCISE !!!!!!!!!!!!! ----------------------------------
# 
# # Function which computes yearly sum for given year
# calculate_yearly_total <- function(climate_var_df, year) {
#   
#   # Subset to relevant year
#   relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
#   varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
#   
#   year <- as.character(year)
#   
#   # Winsorize
#   columns_to_winsorize <- setdiff(names(varyear_df), "idstd")
#   
#   varyear_df[columns_to_winsorize] <- lapply(varyear_df[columns_to_winsorize], function(x) {
#     x_winsorized <- winsorize(x, probs = c(0.01, 0.99), na.rm = TRUE)
#     return(x_winsorized)
#   })
#   
#   # Sum all columns except idstd
#   result <- varyear_df %>%
#     mutate(!!year := rowSums(select(., -idstd), na.rm = TRUE)) %>% 
#     select(idstd, !!year)
#   
#   return(result)
# }
# 
# # Function which identifies yearly max for given year
# calculate_yearly_max <- function(climate_var_df, year) {
#   
#   # Subset to relevant year
#   relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
#   varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
#   
#   year <- as.character(year)
#   
#   # Winsorize
#   columns_to_winsorize <- setdiff(names(varyear_df), "idstd")
#   
#   varyear_df[columns_to_winsorize] <- lapply(varyear_df[columns_to_winsorize], function(x) {
#     x_winsorized <- winsorize(x, probs = c(0.01, 0.99), na.rm = TRUE)
#     return(x_winsorized)
#   })
#   
#   # Find max of all columns except idstd
#   matrix_data <- varyear_df[,-1]
#   row_max_values <- rowMaxs(as.matrix(matrix_data), na.rm = TRUE)
#   
#   # Add max to df
#   varyear_df[[year]] <- row_max_values
#   
#   # Drop the monthly columns
#   result <- varyear_df %>% 
#     select(idstd, !!year)
#   
#   return(result)
# }
# 
# 
# # Putting it all together - yearly totals or max, avg by decade, diff
# yearlymetric_decadeavg_diff <- function(variable, metric){
#   
#   # Subset to only climate var
#   var_df <- climate_var_subset(variable)
#   
#   
#   # Calculate yearly totals for each year
#   wrapper <- function(year) {
#     metric(climate_var_df = var_df, year)
#   }
#   
#   var_totals <- lapply(entire_period, wrapper)
#   
#   var_totals <- Reduce(function(x, y) merge(x, y, by = "idstd", all = TRUE), var_totals)
#   
#   
#   # Calculate period means and difference
#   var_means <- period_means(var_totals)
#   
#   var_means <- difference(var_means)
#   
#   
#   # Merge with sf_joined and weights
#   sf_var <- left_join(sf_joined, var_means, by = "idstd")
#   
#   sf_var <- left_join(sf_var, weights, by = "idstd")
#   
#   # Find average difference per grid
#   sf_var <- sf_var %>% 
#     group_by(geometry) %>% 
#     summarize(HeatVar = weighted.mean(difference, wt_rs, na.rm = TRUE))
# }
# 
# #------------------------------- 5. TEMPERATURE --------------------------------
# 
# #Hotdays
# sf_hotdays_w <- yearlymetric_decadeavg_diff("hotdays", calculate_yearly_total)
# 
# #spei
# sf_spei_w <- yearlymetric_decadeavg_diff("spei", calculate_yearly_total)
# 
# 
# #-------------------------------- 6. RAINFALL ----------------------------------
# 
# #Cwd
# sf_cwd_w <- yearlymetric_decadeavg_diff("cwd", calculate_yearly_max)
# 
# #Precip - yearly total
# sf_precip_w <- yearlymetric_decadeavg_diff("precip", calculate_yearly_total)
# 
# #sf_precip <- yearly_extreme_decade_diff_compute(WHICH VAR?)
# 
# 
# #------------------------------- 7. DROUGHT ------------------------------------
# 
# #Drydays
# sf_drydays_w <- yearlymetric_decadeavg_diff("drydays", calculate_yearly_total)
# 
# #Cdd
# sf_cdd_w <- yearlymetric_decadeavg_diff("cdd", calculate_yearly_max)
# 
