#-------------------------------------------------------------------------------
# Script: climate_data_clean_longrun.R
# Author: Nolan Young Zabala
# Description: - format climate data in usable way
#              - compute metrics like old averages/changes/etc
#              - output data for use in dashboard
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(sf)
library(matrixStats)
library(ggplot2)
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
  select(idstd, wt_rs)

# Get rid of vars I don't need
not_needed <- c("X", "a2x", "a3ax", "size", "a7", "b6", "stra_sector", "b4", "b4a")

climate <- climate %>% 
  select(-all_of(not_needed))

# Replace NAs
climate[climate == -99] <- NA
climate[climate == -9] <- NA
climate[climate == -7] <- NA


#------------------------ 3. PRE/CURRENT/ENTIRE PERIOD -------------------------

# Pre-decade
decade1 <- 2001:2010

# Current period
current_period <- 2011:2021

# Lon-run period
longrun <- 1981:2010

# Entire period
longer_entire <- 1981:2021
shorter_entire <- 2001:2021


#---------------------------- 4. GENERAL FUNCTIONS -----------------------------


# METHOD:

# For hotdays, precip, and spei: compare long-run mean (1981-2010) with this decade's weighted average
# For drydays, cdd, and cwd: find difference in weighted avgs between decade


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
  
  # Winsorize
  columns_to_winsorize <- setdiff(names(varyear_df), "idstd")
  
  varyear_df[columns_to_winsorize] <- lapply(varyear_df[columns_to_winsorize], function(x) {
    x_winsorized <- winsorize(x, probs = c(0.01, 0.99), na.rm = TRUE)
    return(x_winsorized)
  })
  
  # Sum all columns except idstd
  result <- varyear_df %>%
    mutate(!!year := rowSums(select(., -idstd), na.rm = TRUE)) %>% 
    select(idstd, !!year)
  
  return(result)
}

# Function which identifies yearly max for given year
calculate_yearly_max <- function(climate_var_df, year) {
  
  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Winsorize
  columns_to_winsorize <- setdiff(names(varyear_df), "idstd")
  
  varyear_df[columns_to_winsorize] <- lapply(varyear_df[columns_to_winsorize], function(x) {
    x_winsorized <- winsorize(x, probs = c(0.01, 0.99), na.rm = TRUE)
    return(x_winsorized)
  })
  
  # Find max of all columns except idstd
  matrix_data <- varyear_df[,-1]
  row_max_values <- rowMaxs(as.matrix(matrix_data), na.rm = TRUE)
  
  # Add max to df
  varyear_df[[year]] <- row_max_values
  
  # Drop the monthly columns
  result <- varyear_df %>% 
    select(idstd, !!year)
  
  return(result)
}


# Period means function
period_means <- function(df, pre_period){
  
  result <- df %>%
    mutate(pre_mean = rowMeans(select(., as.character(pre_period)), na.rm = TRUE),
           current_mean = rowMeans(select(., as.character(current_period)), na.rm = TRUE)) %>%
    select(idstd, pre_mean, current_mean)  
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


# Putting it all together - yearly totals or max, avg by decade, % change
yearlymetric_decadeavg_pcntchange <- function(variable, metric){
  
  # Subset to only climate var
  var_df <- climate_var_subset(variable)
  
  # Define periods depending on var
  if (variable %in% c("hotdays", "precip", "spei")){
    pre_period <- longrun
    entire_period <- longer_entire
  }
  
  if (variable %in% c("drydays", "cdd", "cwd")){
    pre_period <- decade1
    entire_period <- shorter_entire
  }
  
  
  # Calculate yearly totals for each year
  wrapper <- function(year) {
    metric(climate_var_df = var_df, year)
  }
  
  var_totals <- lapply(entire_period, wrapper)
  
  var_totals <- Reduce(function(x, y) merge(x, y, by = "idstd", all = TRUE), var_totals)
  
  
  # Calculate period means and difference, depending on var type
  var_means <- period_means(var_totals, pre_period)
  
  var_means <- difference(var_means)
  
  
  # Merge with sf_joined and weights
  sf_var <- left_join(sf_joined, var_means, by = "idstd")
  
  sf_var <- left_join(sf_var, weights, by = "idstd")
  
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


#------------------------------- 5. VARS --------------------------------

#Hotdays
sf_hotdays_lr <- yearlymetric_decadeavg_pcntchange("hotdays", calculate_yearly_total)

#spei
sf_spei_lr <- yearlymetric_decadeavg_pcntchange("spei", calculate_yearly_total)

#Precip
sf_precip_lr <- yearlymetric_decadeavg_pcntchange("precip", calculate_yearly_total)





Long-run vs Decade
=======================================================================
  
  Column {.tabset .tabset-fade data-width=500}
-----------------------------------------------------------------------
  
  ### Hot Days
  
  ```{r}
legend_title <- "Difference in weighted average"

heatplot_climate(sf_hotdays_lr, "Change in Average Yearly Hot Days\nPre (1981-2010) to Current (2011-21) Period", legend_title)
```

### SPEI

```{r}
heatplot_climate(sf_spei_lr, "Change in Average Yearly SPEI\nPre (1981-2010) to Current (2011-21) Period", legend_title)
```

### Precipitation

```{r}
heatplot_climate(sf_precip_lr, "Change in Average Yearly Precip\nPre (1981-2010) to Current (2011-21) Period", legend_title)
```

Column {.tabset .tabset-fade data-width=500}
-----------------------------------------------------------------------
  
