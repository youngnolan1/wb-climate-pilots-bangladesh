#-------------------------------------------------------------------------------
# Script: regression_analysis.R
# Author: Nolan Young Zabala
# Description: - prepare data
#              - regress revenue on firm characteristics with interaction terms
#              - standardize coefficients and plot them with confidence intervals
#-------------------------------------------------------------------------------


#------------------------------- 1. SET UP -------------------------------------

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")

# Load libraries
library(dplyr)
library(tidyr)
library(fastDummies)
library(QuantPsyc)

# Load processed data (created by "regression_analysis_dataprep.R")
reg_master <- read.csv("intermediate_data/BanglaRegAnalysisData.csv")

# Define reference category for obstacle vars
obstacle_vars <- grep("^obstacle", names(reg_master), value = TRUE)

for (var in obstacle_vars){
  reg_master[[var]] <- factor(reg_master[[var]], levels = c("No", "Minor", "Moderate", "Major", "VerySevere"))
}

# Drop NAs for investment
investment_reg_data <- reg_master[complete.cases(reg_master$total_invest_fixed_asset), ]

#----------------------------- 2. TEMPERATURE ----------------------------------

# Run reg
temp_model <- lm(total_invest_fixed_asset ~ avg_temp + avg_temp:firm_age + avg_temp:size + avg_temp:sampling_region + avg_temp:pcnt_owned_by_largest_owner + avg_temp:manager_experience_yrs + avg_temp:top_manager_female + avg_temp:market_served + avg_temp:obstacle_electricity + avg_temp:obstacle_transport + avg_temp:obstacle_traderegulations + avg_temp:obstacle_businesslicensing + avg_temp:obstacle_accesstofinance + avg_temp:obstacle_laborregulations + firm_age + size + sampling_region + pcnt_owned_by_largest_owner + manager_experience_yrs + top_manager_female + market_served + obstacle_electricity + obstacle_transport + obstacle_traderegulations + obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations, data = investment_reg_data, weights = wt)

# Store results
temp_model_results <- data.frame(coef = coef(temp_model), ci_lower = confint(temp_model)[, 1], ci_upper = confint(temp_model)[, 2])

# Extract interaction terms only
temp_interactions <- temp_model_results[grepl(":", rownames(temp_model_results)), ]
rownames(temp_interactions) <- sub("^avg_temp:", "", rownames(temp_interactions))
temp_interactions$var <- rownames(temp_interactions)
rownames(temp_interactions) <- 1:nrow(temp_interactions)


#---------------------- 3. TEMPERATURE VOLATILITY ------------------------------

# Run reg
tempvolatility_model <- lm(total_invest_fixed_asset ~ temp_volatility + temp_volatility:firm_age + temp_volatility:size + temp_volatility:sampling_region + temp_volatility:pcnt_owned_by_largest_owner + temp_volatility:manager_experience_yrs + temp_volatility:top_manager_female + temp_volatility:market_served + temp_volatility:obstacle_electricity + temp_volatility:obstacle_transport + temp_volatility:obstacle_traderegulations + temp_volatility:obstacle_businesslicensing + temp_volatility:obstacle_accesstofinance + temp_volatility:obstacle_laborregulations + firm_age + size + sampling_region + pcnt_owned_by_largest_owner + manager_experience_yrs + top_manager_female + market_served + obstacle_electricity + obstacle_transport + obstacle_traderegulations + obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations, data = investment_reg_data, weights = wt)

# Store results
tempvolatility_model_results <- data.frame(coef = coef(tempvolatility_model), ci_lower = confint(tempvolatility_model)[, 1], ci_upper = confint(tempvolatility_model)[, 2])

# Extract interaction terms
tempvolatility_interactions <- tempvolatility_model_results[grepl(":", rownames(tempvolatility_model_results)), ]
rownames(tempvolatility_interactions) <- sub("^temp_volatility:", "", rownames(tempvolatility_interactions))
tempvolatility_interactions$var <- rownames(tempvolatility_interactions)
rownames(tempvolatility_interactions) <- 1:nrow(tempvolatility_interactions)


#--------------------------------- 4. HOT DAYS ---------------------------------

# Run reg
hotdays_model <- lm(total_invest_fixed_asset ~ total_hotdays + total_hotdays:firm_age + total_hotdays:size + total_hotdays:sampling_region + total_hotdays:pcnt_owned_by_largest_owner + total_hotdays:manager_experience_yrs + total_hotdays:top_manager_female + total_hotdays:market_served + total_hotdays:obstacle_electricity + total_hotdays:obstacle_transport + total_hotdays:obstacle_traderegulations + total_hotdays:obstacle_businesslicensing + total_hotdays:obstacle_accesstofinance + total_hotdays:obstacle_laborregulations + firm_age + size + sampling_region + pcnt_owned_by_largest_owner + manager_experience_yrs + top_manager_female + market_served + obstacle_electricity + obstacle_transport + obstacle_traderegulations + obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations, data = investment_reg_data, weights = wt)

# Store results
hotdays_model_results <- data.frame(coef = coef(hotdays_model), ci_lower = confint(hotdays_model)[, 1], ci_upper = confint(hotdays_model)[, 2])

# Extract interaction terms
hotdays_interactions <- hotdays_model_results[grepl(":", rownames(hotdays_model_results)), ]
rownames(hotdays_interactions) <- sub("^total_hotdays:", "", rownames(hotdays_interactions))
hotdays_interactions$var <- rownames(hotdays_interactions)
rownames(hotdays_interactions) <- 1:nrow(hotdays_interactions)

#-------------------------------- 5. SPEI --------------------------------------

# Run reg
spei_model <- lm(total_invest_fixed_asset ~ avg_spei + avg_spei:firm_age + avg_spei:size + avg_spei:sampling_region + avg_spei:pcnt_owned_by_largest_owner + avg_spei:manager_experience_yrs + avg_spei:top_manager_female + avg_spei:market_served + avg_spei:obstacle_electricity + avg_spei:obstacle_transport + avg_spei:obstacle_traderegulations + avg_spei:obstacle_businesslicensing + avg_spei:obstacle_accesstofinance + avg_spei:obstacle_laborregulations + firm_age + size + sampling_region + pcnt_owned_by_largest_owner + manager_experience_yrs + top_manager_female + market_served + obstacle_electricity + obstacle_transport + obstacle_traderegulations + obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations, data = investment_reg_data, weights = wt)

# Store results
spei_model_results <- data.frame(coef = coef(spei_model), ci_lower = confint(spei_model)[, 1], ci_upper = confint(spei_model)[, 2])

# Extract interaction terms only
spei_interactions <- spei_model_results[grepl(":", rownames(spei_model_results)), ]
rownames(spei_interactions) <- sub("^avg_spei:", "", rownames(spei_interactions))
spei_interactions$var <- rownames(spei_interactions)
rownames(spei_interactions) <- 1:nrow(spei_interactions)

