#-------------------------------------------------------------------------------
# Script: benchmarking_firms.R
# Author: Nolan Young Zabala
# Description: - load 2013 data for Bangladesh
#              - run regressions and calculate residuals
#              - plot residuals
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(haven)
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)


# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------


#master <- read_dta("raw_data/master_analysis.dta")


#bangla13 <- read.csv("intermediate_data/Bangla13_Firm_withGDP.csv")

#--------------------------- 3. CREATE SECTORxYEAR  ----------------------------

# Create secyr variable
master <- master %>%
  mutate(secyr = sprintf("%02.0f%02.0f", isic, wbes_year),
         sy = as.factor(match(secyr, unique(secyr))))

# Create cysec variable
master <- master %>%
  mutate(counsec = sprintf("%02.0f%02.0f", c, isic),
         cs = as.factor(match(counsec, unique(counsec))))

# Encode country variable
master <- master %>%
  mutate(cy = as.factor(match(country, unique(country))))

#----------------------- 4. CALCULATE CLIMATE VARS -----------------------------

# TEMPERATURE
# Calculate mean temperature
master$avg_temp <- rowMeans(master[, c("mean_temp_m1", "mean_temp_m2", "mean_temp_m3", "mean_temp_m4", "mean_temp_m5", "mean_temp_m6", "mean_temp_m7", "mean_temp_m8", "mean_temp_m9", "mean_temp_m10", "mean_temp_m11", "mean_temp_m12")])

# Winsorize avg_temp
master$avg_temp <- pmin(quantile(master$avg_temp, 0.99, na.rm = TRUE), pmax(master$avg_temp, quantile(master$avg_temp, 0.01, na.rm = TRUE)))

# Calculate t2 and t3
master$t2 <- master$avg_temp^2
master$t3 <- master$avg_temp^3


# PRECIPITATION
# Calculate mean precip
master$avg_precip <- rowMeans(master[, c("mean_precip_m1", "mean_precip_m2", "mean_precip_m3", "mean_precip_m4", "mean_precip_m5", "mean_precip_m6", "mean_precip_m7", "mean_precip_m8", "mean_precip_m9", "mean_precip_m10", "mean_precip_m11", "mean_precip_m12")])

# Winsorize avg_precip
master$avg_precip <- pmin(quantile(master$avg_precip, 0.99, na.rm = TRUE), pmax(master$avg_precip, quantile(master$avg_precip, 0.01, na.rm = TRUE)))

# Calculate p2 and p3
master$p2 <- master$avg_precip^2
master$p3 <- master$avg_precip^3


# HOTDAYS
# Calculate total hotdays
master$total_hotdays <- rowSums(master[, c("hotdays_m1", "hotdays_m2", "hotdays_m3", "hotdays_m4", 
                                           "hotdays_m5", "hotdays_m6", "hotdays_m7", "hotdays_m8", 
                                           "hotdays_m9", "hotdays_m10", "hotdays_m11", "hotdays_m12")])


# Winsorize total_hotdays
master$total_hotdays <- pmin(quantile(master$total_hotdays, 0.99, na.rm = TRUE), pmax(master$total_hotdays, quantile(master$total_hotdays, 0.01, na.rm = TRUE)))


# Calculate hd2 and hd3
master$hd2 <- master$total_hotdays^2
master$hd3 <- master$total_hotdays^3


#------------------------- 5. PREPARE DEPENDENT VARS ---------------------------

master <- master %>%
  mutate(d2 = ifelse(d2 < 0, NA, d2)) %>% 
  mutate(logRev = log(d2))


#-------------------------- 6. CREATE TERCILES ---------------------------------

# Create terciles for lr_temp and generate a new column tt
master <- master %>%
  mutate(tt = ntile(lr_temp, 3))

# Create terciles for lr_precip and generate a new column pt
master <- master %>%
  mutate(pt = ntile(lr_precip, 3))

# Create terciles for lr_hd and generate a new column hdt
master <- master %>% 
  mutate(hdt = ntile(lr_hd, 3))

# Create quantiles for GDPpc_ppp and generate a new column im
master <- master %>%
  mutate(im = ntile(GDPpc_ppp, 2))


#-------------------- 7. ESTIMATE RESPONSE FUNCTIONS ----------------------------

# TEMPERATURE
# Most frequent combo for Bangla firms is 3/1 - third tercile for temperature, 
# and below-average GDP per capita.

# Subset to only these firms
response <- master %>% 
  filter(tt == 3 & im == 1) %>% 
  drop_na(logRev) %>% 
  filter(logRev >= 0)

# Create columns for predictions and residuals
response$temp_pred <- NA
response$temp_resid <- NA

# Estimate fixed-effects regression
model <- feols(logRev ~ avg_temp + t2 + t3 + lr_temp | cy + sy + cs, data = response)
    
# Save predictions and residuals
response$temp_pred <- predict(model)
response$temp_resid <- residuals(model)


# PRECIPITATION
# Most frequent combo for Bangla firms is 3/1 - third tercile for precip, 
# and below-average GDP per capita. Same firms as temp, so no further subsetting needed.

# Create columns for predictions and residuals
response$precip_pred <- NA
response$precip_resid <- NA

# Estimate fixed-effects regression
model <- feols(logRev ~ avg_precip + p2 + p3 + lr_precip | cy + sy + cs, data = response)

# Save predictions and residuals
response$precip_pred <- predict(model)
response$precip_resid <- residuals(model)


# HOT DAYS
# Most frequent combo for Bangla firms is 3/1 - third tercile for hotdays, 
# and below-average GDP per capita. Some firms in other combos, though, so need
# to re-subset master df. Merge results back in after. (For Mozam combo is 2/1).
response_hd <- master %>% 
  filter(im == 1 & hdt == 3) %>% 
  drop_na(logRev) %>% 
  filter(logRev >= 0)

# Create columns for predictions and residuals
response_hd$hotdays_pred <- NA
response_hd$hotdays_resid <- NA

# Estimate fixed-effects regression
model <- feols(logRev ~ total_hotdays + hd2 + hd3 + lr_hd | cy + sy + cs, data = response_hd)

# Save predictions and residuals
response_hd$hotdays_pred <- predict(model)
response_hd$hotdays_resid <- residuals(model)


# MERGE
response_hd <- response_hd %>% 
  select(idstd, hotdays_pred, hotdays_resid)

response <- left_join(response, response_hd, by = "idstd")

# Subset to Bangladeshi firms
bangla <- response %>% 
  filter(country == "Bangladesh2013")


#---------------------- 8. QUANTILES OF RESIDUALS ------------------------------

bangla <- bangla %>%
  mutate(temp_resid_quartile = ntile(temp_resid, 4)) %>% 
  mutate(precip_resid_quartile = ntile(precip_resid, 4)) %>% 
  mutate(hotdays_resid_quartile = ntile(hotdays_resid, 4))
  

# Write to csv
#write.csv(bangla, "intermediate_data/Bangla_ResidQuart.csv")

#---------------------- 9. PLOTTING RESIDUALS ----------------------------------

# See 7_benchmarking_plots.Rmd and bangladesh_residual_dashboard.Rmd

