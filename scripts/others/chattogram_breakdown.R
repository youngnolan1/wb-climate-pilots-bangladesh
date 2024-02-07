#-------------------------------------------------------------------------------
# Script: chattogram_breakdown.R
# Author: Nolan Young Zabala
# Description: - breakdown Chattogram firms by size, sector, and age
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(sf)
library(ggplot2)

# Set working directory - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh")


#-------------------------------- 2. LOAD DATA ---------------------------------

# Load more granular Bangladesh grids
grids <- read_sf("raw_data/shapefiles/gadm41_BGD_3.shp") %>% 
  select(geometry) %>% 
  mutate(grid_id = row_number())

# Load firms
firms <- read.csv("intermediate_data/Bangla13_Firm.csv")


#------------------------- 3. PREPARE AND MERGE DATA ---------------------------

# Replace NAs
firms[firms == -99] <- NA
firms[firms == -9] <- NA
firms[firms == -7] <- NA

# Filter to just Chattogram and Comilla division
# No data for Feni between unfortunately
chittagong <- firms %>% 
  filter(a3ax == "Chittagong")

# Convert firm dataframe to sf object using grids as reference
sf_chittagong <- st_as_sf(chittagong, 
                          coords = c("lon_mask", "lat_mask"), 
                          crs = st_crs(grids)) 

# Assign firms to grids
sf_grids <- st_join(grids,
                    sf_chittagong,
                    left = TRUE)

# Filter grids to only Chittagong division
division <- sf_grids %>% 
  filter(between(grid_id, 50, 150))

#---------------------------- 3. SUMMARY PIES ----------------------------------

# Create summary df
df_stats <- division %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  filter(!is.na(idstd))

# Assign isic sectors
df_stats <- df_stats %>%
  mutate(isic_sector = case_when(
    between(isic, 1, 2) ~ "Agriculture, hunting and forestry",
    isic == 5 ~ "Fishing",
    between(isic, 10, 14) ~ "Mining and quarrying",
    between(isic, 15, 37) ~ "Manufacturing",
    between(isic, 40, 41) ~ "Electricity, gas and water supply",
    isic == 45 ~ "Construction",
    between(isic, 50, 52) ~ "Wholesale/retail trade",
    isic == 55 ~ "Restaurants/hotels",
    between(isic, 60, 64) ~ "Transport",
    between(isic, 65, 67) ~ "Financial intermediation",
    between(isic, 70, 74) ~ "Real estate, renting and business activities",
    isic == 80 ~ "Education",
    isic == 85 ~ "Health and social work",
    between(isic, 90, 93) ~ "Other community, social and personal service activities"
  ))

# Size
size_counts <- as.data.frame(table(df_stats$size)) %>% 
  rename(Size = Var1) %>% 
  mutate(Size = case_when(
    Size == 1 ~ "Small",
    Size == 2 ~ "Medium",
    Size == 3 ~ "Large"
  )) %>% 
  mutate(Prop = round((Freq/269) * 100))

ggplot(data = size_counts, aes(x = "", y = Prop, fill = Size)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(Size, "\n", Prop, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Small" = "#00BA38", "Medium" = "#619CFF", "Large" = "#F8766D")) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text = element_blank(),  
    axis.title = element_blank()  
  )

# Sector
sector_counts <- as.data.frame(table(df_stats$isic_sector)) %>% 
  rename(Sector = Var1) %>% 
  mutate(Prop = round((Freq/269) * 100))

ggplot(data = sector_counts, aes(x = "", y = Prop, fill = Sector)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(
    legend.position = "right",  # Adjust legend position
    axis.text = element_blank(),  # Hide axis text
    axis.title = element_blank()  # Hide axis title
  )

# Age
age_counts <- df_stats %>% 
  select(idstd, b5) %>% 
  filter(b5 >= 0) %>% 
  mutate(firm_age = 2013 - b5) %>% 
  mutate(firm_age_bucket = case_when(
    between(firm_age, 0, 5) ~ "0-5yrs",
    between(firm_age, 6, 15) ~ "6-15yrs",
    firm_age >= 16 ~ "Over 15yrs"
  ))

age_counts <- as.data.frame(table(age_counts$firm_age_bucket)) %>% 
  rename(Age = Var1) %>% 
  mutate(Prop = round((Freq/269) * 100))

ggplot(data = age_counts, aes(x = "", y = Prop, fill = Age)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(Age, "\n", Prop, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("0-5yrs" = "#00BA38", "6-15yrs" = "#619CFF", "Over 15yrs" = "#F8766D")) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text = element_blank(),  
    axis.title = element_blank()  
  )



#-------------------------------- 4. PLOTS -------------------------------------

# General plot function
heatplot <- function(df, plot_title, legend_title){   
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient(low = "lightgrey", high = "red") +     
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}

# Size
chitta_size <- division %>% 
  select(geometry, size) %>% 
  group_by(geometry) %>% 
  summarise(HeatVar = mean(size, na.rm = TRUE))

heatplot(chitta_size, NULL, "Avg firm size")

# Sector
grids <- division %>% 
  select(idstd, geometry) %>% 
  left_join(df_stats, by = "idstd")

chitta_sector <- grids %>% 
  select(geometry, isic_sector) %>% 
  filter(isic_sector == "Manufacturing") %>% 
  group_by(geometry) %>% 
  summarise(HeatVar = n())

chattogrids <- chitta_size %>% 
  select(geometry) %>% 
  st_join(chitta_sector)

heatplot(chattogrids, "Manufacturing firms", "Firm count")

# Age
chitta_age <- division %>% 
  select(idstd, b5) %>% 
  mutate(firm_age = 2013 - b5) %>% 
  group_by(geometry) %>% 
  summarise(HeatVar = mean(firm_age, na.rm = TRUE))

heatplot(chitta_age, NULL, "Avg firm age")





