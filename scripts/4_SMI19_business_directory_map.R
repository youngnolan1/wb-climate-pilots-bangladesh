#-------------------------------------------------------------------------------
# Script: 4_SMI19_business_directory_map.R
# Author: Nolan Young Zabala
# Description: 
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(sf)
library(readxl)

setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")

#-------------------------------- 2. LOAD DATA ---------------------------------

# Load SMI 2019 business directory
business <- read_excel("raw_data/Business _Directory_2019.xlsx")

# BSIC Section C - Manufacturing (Source: https://bbs.portal.gov.bd/sites/default/files/files/bbs.portal.gov.bd/page/745673c8_c7ed_49bc_a4e2_e7b05fe7a9d4/2021-06-08-08-39-6be22d0db12b4087042cca9e0ff9222e.pdf)
bsic_manufacturing <- 1010:3320

business <- business %>%
  filter(BSIC %in% bsic_manufacturing) %>% 
  select(Establishment_cd, zilla_id, zilla_name_comite_eng, BSIC) %>% 
  rename(ZillaName = zilla_name_comite_eng)
  
# Load shapefile
shapefile <- read_sf("raw_data/shapefiles/gadm41_BGD_2.shp") 


#------------------------- 3. MERGE USING ZILLA NAME ---------------------------

# Capitalize shapefile Zilla names
shapefile <- shapefile %>% 
  rename(ZillaName = NAME_2)

shapefile$ZillaName <- toupper(shapefile$ZillaName)

# Change Zilla Names to match
business$ZillaName <- gsub("COXS BAZAR", "COX'S BAZAR", business$ZillaName)
business$ZillaName <- gsub("BARISHAL", "BARISAL", business$ZillaName)
business$ZillaName <- gsub("BOGURA", "BOGRA", business$ZillaName)
business$ZillaName <- gsub("BRAHMANBARIA", "BRAHAMANBARIA", business$ZillaName)
business$ZillaName <- gsub("CHATTOGRAM", "CHITTAGONG", business$ZillaName)
business$ZillaName <- gsub("CHAPAINABABGANJ", "NAWABGANJ", business$ZillaName)
business$ZillaName <- gsub("CUMILLA", "COMILLA", business$ZillaName)
business$ZillaName <- gsub("JASHORE", "JESSORE", business$ZillaName)
business$ZillaName <- gsub("MOULVIBAZAR", "MAULVIBAZAR", business$ZillaName)


# Merge
merged <- left_join(shapefile, business, by = "ZillaName") %>% 
  select(Establishment_cd, zilla_id, ZillaName, BSIC, geometry)


#-------------------------- 4. PREPARE FOR MAPPING -----------------------------

# Set var to 1 to count
merged$Establishment_cd[!is.na(merged$Establishment_cd)] <- 1
merged$Establishment_cd <- as.numeric(merged$Establishment_cd)

# Group by grid and count
smi19_firm_count <- merged %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(Establishment_cd))