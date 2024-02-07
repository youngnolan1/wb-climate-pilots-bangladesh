#-------------------------------------------------------------------------------
# Script: residual_quartile_map_prep.R
# Author: Nolan Young Zabala
# Description: - load Bangladesh residual data
#              - calculate quartiles
#              - merge with geo-linked firm data
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(dplyr)
library(sf)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")

#-------------------------------- 2. LOAD DATA ---------------------------------

# Load residual data
resid_quart <- read.csv("intermediate_data/Bangla_ResidQuart.csv")

# Load geo-linked firm data
sf_joined <- read_sf("intermediate_data/sf_joined.shp")


#------------------------------- 3. MERGE DATA ---------------------------------

# TEMPERATURE
# Worst affected firms
temp_resid_quart1 <- resid_quart %>% 
  select(idstd, temp_resid_quartile, wt_rs) %>% 
  filter(temp_resid_quartile == 1)

# Firms doing the best
temp_resid_quart4 <- resid_quart %>% 
  select(idstd, temp_resid_quartile, wt_rs) %>% 
  filter(temp_resid_quartile == 4)

# Merge each
temp_resid_quart1 <- left_join(sf_joined, temp_resid_quart1, by = "idstd") %>% 
  select(idstd, geometry, temp_resid_quartile, wt_rs)

temp_resid_quart4 <- left_join(sf_joined, temp_resid_quart4, by = "idstd") %>% 
  select(idstd, geometry, temp_resid_quartile, wt_rs)

# Group by grid and count
temp_resid_quart1 <- temp_resid_quart1 %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))

temp_resid_quart4 <- temp_resid_quart4 %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))


# HOTDAYS
# Worst affected firms
hotdays_resid_quart1 <- resid_quart %>% 
  select(idstd, hotdays_resid_quartile, wt_rs) %>% 
  filter(hotdays_resid_quartile == 1)

# Firms doing the best
hotdays_resid_quart4 <- resid_quart %>% 
  select(idstd, hotdays_resid_quartile, wt_rs) %>% 
  filter(hotdays_resid_quartile == 4)

# Merge each
hotdays_resid_quart1 <- left_join(sf_joined, hotdays_resid_quart1, by = "idstd") %>% 
  select(idstd, geometry, hotdays_resid_quartile, wt_rs)

hotdays_resid_quart4 <- left_join(sf_joined, hotdays_resid_quart4, by = "idstd") %>% 
  select(idstd, geometry, hotdays_resid_quartile, wt_rs)

# Group by grid and count
hotdays_resid_quart1 <- hotdays_resid_quart1 %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))

hotdays_resid_quart4 <- hotdays_resid_quart4 %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))


# PRECIPITATION
# Worst affected firms
precip_resid_quart1 <- resid_quart %>% 
  select(idstd, precip_resid_quartile, wt_rs) %>% 
  filter(precip_resid_quartile == 1)

# Firms doing the best
precip_resid_quart4 <- resid_quart %>% 
  select(idstd, precip_resid_quartile, wt_rs) %>% 
  filter(precip_resid_quartile == 4)

# Merge each
precip_resid_quart1 <- left_join(sf_joined, precip_resid_quart1, by = "idstd") %>% 
  select(idstd, geometry, precip_resid_quartile, wt_rs)

precip_resid_quart4 <- left_join(sf_joined, precip_resid_quart4, by = "idstd") %>% 
  select(idstd, geometry, precip_resid_quartile, wt_rs)

# Group by grid and count
precip_resid_quart1 <- precip_resid_quart1 %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))

precip_resid_quart4 <- precip_resid_quart4 %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(wt_rs, na.rm = TRUE)) %>% 
  mutate(HeatVar = HeatVar/sum(HeatVar)) %>% 
  mutate(HeatVar = case_when(
    HeatVar == 0 ~ NA,
    HeatVar != 0 ~ HeatVar
  ))
