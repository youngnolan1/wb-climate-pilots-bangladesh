#-------------------------------------------------------------------------------
# Script: SMI19_sector_breakdown.R
# Author: Nolan Young Zabala
# Description: 
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(dplyr)
library(sf)
library(readxl)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")

#-------------------------------- 2. LOAD DATA ---------------------------------

# Load SMI 2019 business directory
business <- read_excel("raw_data/Business _Directory_2019.xlsx")


#------------------------------- 3. CLEAN DATA ---------------------------------

# Subset and rename
business <- business %>%
  select(Establishment_cd, zilla_id, zilla_name_comite_eng, BSIC) %>% 
  rename(ZillaName = zilla_name_comite_eng)

# Define sectors ("section" level from BSIC manual)
business <- business %>% 
  mutate(BSIC = as.numeric(BSIC)) %>% 
  mutate(sector = case_when(
    between(BSIC, 0111, 0322) ~ "A",
    between(BSIC, 0510, 0990) ~ "B",
    between(BSIC, 1010, 3320) ~ "C",
    between(BSIC, 3510, 3530) ~ "D",
    between(BSIC, 3600, 3900) ~ "E",
    between(BSIC, 4100, 4390) ~ "F",
    between(BSIC, 4510, 4799) ~ "G",
    between(BSIC, 4911, 5320) ~ "H",
    between(BSIC, 5510, 5630) ~ "I",
    between(BSIC, 5811, 6399) ~ "J",
    between(BSIC, 6411, 6630) ~ "K",
    between(BSIC, 6810, 6820) ~ "L",
    between(BSIC, 6910, 7490) ~ "M",
    between(BSIC, 7500, 8299) ~ "N",
    between(BSIC, 8411, 8430) ~ "O",
    between(BSIC, 8510, 8550) ~ "P",
    between(BSIC, 8610, 9499) ~ "Q",
    between(BSIC, 9511, 9529) ~ "R",
    between(BSIC, 9601, 9609) ~ "S",
    between(BSIC, 9700, 9820) ~ "T",
    BSIC == 9900 ~ "U"
  ))

#------------------------------ 3. LOAD SHAPEFILE ------------------------------

# Load shapefile
shapefile <- read_sf("raw_data/shapefiles/gadm41_BGD_2.shp") 

# Capitalize shapefile Zilla names
shapefile <- shapefile %>% 
  rename(ZillaName = NAME_2) %>% 
  select(ZillaName, geometry)

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

merged <- business

#------------------- 4. FIRM COUNTS BY SECTION (2-DIGIT) -----------------------

# Set var to 1 to count
merged$Establishment_cd[!is.na(merged$Establishment_cd)] <- 1
merged$Establishment_cd <- as.numeric(merged$Establishment_cd)

# All sectors
total_count <- merged %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# A
a_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "A" ~ 1,
    sector != "A" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()


# B
b_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "B" ~ 1,
    sector != "B" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# C
c_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "C" ~ 1,
    sector != "C" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# D
d_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "D" ~ 1,
    sector != "D" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# E
e_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "E" ~ 1,
    sector != "E" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# F
f_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "F" ~ 1,
    sector != "F" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# G
g_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "G" ~ 1,
    sector != "G" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# H
h_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "H" ~ 1,
    sector != "H" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# I
i_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "I" ~ 1,
    sector != "I" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# J
j_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "J" ~ 1,
    sector != "J" ~ 0)) %>%  
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# K
k_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "K" ~ 1,
    sector != "K" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# L
l_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "L" ~ 1,
    sector != "L" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# M
m_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "M" ~ 1,
    sector != "M" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# N
n_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "N" ~ 1,
    sector != "N" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# O
o_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "O" ~ 1,
    sector != "O" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# P
p_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "P" ~ 1,
    sector != "P" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# Q
q_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "Q" ~ 1,
    sector != "Q" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# R
r_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "R" ~ 1,
    sector != "R" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# S
s_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "S" ~ 1,
    sector != "S" ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# U
u_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    sector == "U" ~ 1,
    sector != "U" ~ 0)) %>%  
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()


#------------------ 5. MANUFACTURING BREAKDOWN (3-DIGIT) -----------------------

# Textiles
textiles_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    between(BSIC, 1311, 1399) ~ 1,
    !between(BSIC, 1311, 1399) ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# Ready-made garments
garments_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    between(BSIC, 1410, 1430) ~ 1,
    !between(BSIC, 1410, 1430) ~ 0)) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()

# Rest of manufacturing
rest_of_manufac_count <- merged %>% 
  mutate(Establishment_cd = case_when(
    between(BSIC, 0, 1009) ~ NA,
    between(BSIC, 1010, 1310) ~ 1,
    between(BSIC, 1311, 1430) ~ NA,
    between(BSIC, 1431, 3320) ~ 1,
    between(BSIC, 3321, 9999) ~ NA)
    ) %>% 
  group_by(ZillaName) %>% 
  summarize(HeatVar = sum(Establishment_cd, na.rm = TRUE)) %>% 
  right_join(shapefile, by = "ZillaName") %>% 
  select(-ZillaName) %>% 
  st_as_sf()
