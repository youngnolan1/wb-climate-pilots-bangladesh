#-------------------------------------------------------------------------------
# Script: regressions_salesperworker_22.R
# Author: Nolan Young Zabala
# Description: - prepare data
#              - regress sales per worker on firm characteristics with interaction terms
#              - standardize coefficients and plot them with confidence intervals
#-------------------------------------------------------------------------------


#------------------------------- 1. SET UP -------------------------------------

# Set working directory
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")

# Load libraries
library(dplyr)

# Load processed data (created by "ES22_explore.R")
reg_master <- read.csv("intermediate_data/BanglaRegAnalysisData22.csv")

# Drop NAs for sales_per_worker
reg_master <- reg_master[complete.cases(reg_master$sales_per_worker), ]

# Re-specify factor levels for obstacle vars
obstacle_columns <- grep("^obstacle", names(reg_master), value = TRUE)
desired_levels <- c("No", "Minor", "Moderate", "Major", "VerySevere")
for (col in obstacle_columns) {
  reg_master[[col]] <- factor(reg_master[[col]], levels = desired_levels)
}



#---------------------------- 2. TEMPERATURE -----------------------------------

# Run model
temp_model <- lm(sales_per_worker ~ temp + temp:firm_age + temp:size + 
                   temp:pcnt_owned_by_largest_owner + temp:manager_experience_yrs +
                   temp:top_manager_female + temp:majority_national_sales + temp:obstacle_electricity +
                   temp:obstacle_transport + temp:obstacle_businesslicensing +
                   temp:obstacle_accesstofinance + temp:obstacle_laborregulations + 
                   temp:checking_savings_account + firm_age + size + pcnt_owned_by_largest_owner + 
                   manager_experience_yrs + top_manager_female + majority_national_sales + obstacle_electricity + obstacle_transport + 
                   obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations + 
                   checking_savings_account,
                 data = reg_master,
                 weights = wstrict)

# Store results and extract interaction terms
temp_model_results <- data.frame(coef = coef(temp_model), ci_lower = confint(temp_model)[, 1], ci_upper = confint(temp_model)[, 2])
temp_interactions <- temp_model_results[grepl(":", rownames(temp_model_results)), ]
rownames(temp_interactions) <- sub("^temp:", "", rownames(temp_interactions))
temp_interactions$var <- rownames(temp_interactions)
rownames(temp_interactions) <- 1:nrow(temp_interactions)


#-------------------------- 3. TEMP VOLATILITY ---------------------------------

# Run model
tempvolatility_model <- lm(sales_per_worker ~ tempvolatility + tempvolatility:firm_age + tempvolatility:size + 
                             tempvolatility:pcnt_owned_by_largest_owner + tempvolatility:manager_experience_yrs +
                             tempvolatility:top_manager_female + tempvolatility:majority_national_sales + tempvolatility:obstacle_electricity +
                             tempvolatility:obstacle_transport + tempvolatility:obstacle_businesslicensing +
                             tempvolatility:obstacle_accesstofinance + tempvolatility:obstacle_laborregulations + 
                             tempvolatility:checking_savings_account + firm_age + size + pcnt_owned_by_largest_owner + 
                             manager_experience_yrs + top_manager_female + majority_national_sales + obstacle_electricity + obstacle_transport + 
                             obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations + 
                             checking_savings_account,
                           data = reg_master,
                           weights = wstrict)

# Store results and extract interaction terms
tempvolatility_model_results <- data.frame(coef = coef(tempvolatility_model), ci_lower = confint(tempvolatility_model)[, 1], ci_upper = confint(tempvolatility_model)[, 2])
tempvolatility_interactions <- tempvolatility_model_results[grepl(":", rownames(tempvolatility_model_results)), ]
rownames(tempvolatility_interactions) <- sub("^tempvolatility:", "", rownames(tempvolatility_interactions))
tempvolatility_interactions$var <- rownames(tempvolatility_interactions)
rownames(tempvolatility_interactions) <- 1:nrow(tempvolatility_interactions)


#------------------------------ 4. HOT DAYS ------------------------------------

# Run model
hotdays_model <- lm(sales_per_worker ~ hotdays + hotdays:firm_age + hotdays:size + 
                             hotdays:pcnt_owned_by_largest_owner + hotdays:manager_experience_yrs +
                             hotdays:top_manager_female + hotdays:majority_national_sales + hotdays:obstacle_electricity +
                             hotdays:obstacle_transport + hotdays:obstacle_businesslicensing +
                             hotdays:obstacle_accesstofinance + hotdays:obstacle_laborregulations + 
                             hotdays:checking_savings_account + firm_age + size + pcnt_owned_by_largest_owner + 
                             manager_experience_yrs + top_manager_female + majority_national_sales + obstacle_electricity + obstacle_transport + 
                             obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations + 
                             checking_savings_account,
                           data = reg_master,
                           weights = wstrict)

# Store results and extract interaction terms
hotdays_model_results <- data.frame(coef = coef(hotdays_model), ci_lower = confint(hotdays_model)[, 1], ci_upper = confint(hotdays_model)[, 2])
hotdays_interactions <- hotdays_model_results[grepl(":", rownames(hotdays_model_results)), ]
rownames(hotdays_interactions) <- sub("^hotdays:", "", rownames(hotdays_interactions))
hotdays_interactions$var <- rownames(hotdays_interactions)
rownames(hotdays_interactions) <- 1:nrow(hotdays_interactions)


#-------------------------------- 5. SPEI --------------------------------------

# Run model
spei_model <- lm(sales_per_worker ~ spei + spei:firm_age + spei:size + 
                             spei:pcnt_owned_by_largest_owner + spei:manager_experience_yrs +
                             spei:top_manager_female + spei:majority_national_sales + spei:obstacle_electricity +
                             spei:obstacle_transport + spei:obstacle_businesslicensing +
                             spei:obstacle_accesstofinance + spei:obstacle_laborregulations + 
                             spei:checking_savings_account + firm_age + size + pcnt_owned_by_largest_owner + 
                             manager_experience_yrs + top_manager_female + majority_national_sales + obstacle_electricity + obstacle_transport + 
                             obstacle_businesslicensing + obstacle_accesstofinance + obstacle_laborregulations + 
                             checking_savings_account,
                           data = reg_master,
                           weights = wstrict)

# Store results and extract interaction terms
spei_model_results <- data.frame(coef = coef(spei_model), ci_lower = confint(spei_model)[, 1], ci_upper = confint(spei_model)[, 2])
spei_interactions <- spei_model_results[grepl(":", rownames(spei_model_results)), ]
rownames(spei_interactions) <- sub("^spei:", "", rownames(spei_interactions))
spei_interactions$var <- rownames(spei_interactions)
rownames(spei_interactions) <- 1:nrow(spei_interactions)
