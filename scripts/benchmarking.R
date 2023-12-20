#-------------------------------------------------------------------------------
# Script: benchmarking.R
# Author: Nolan Young Zabala
# Description: perform benchmarking exercise outlined by Megan
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(dplyr)
library(ggplot2)

# Set working directory 
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh/raw_data")

#-------------------------------- 2. LOAD DATA ---------------------------------

# Load enterprise data
bangla13 <- read.csv("Bangla_13.csv")


setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-turkiye/raw_data")
turk <- read.csv("Turk_19.csv")

#---------------------- 3. BENCHMARKING STEP-BY-STEP ---------------------------

# Copy
bangla <- bangla13

# 1. Dummy vars for above/below median GDP

# WAITING FOR ANSWER FROM MEGAN - WHERE IS THE GDP VARIABLE?



