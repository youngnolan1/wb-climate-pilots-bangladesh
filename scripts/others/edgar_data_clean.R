#-------------------------------------------------------------------------------
# Script: edgar_data_clean.R
# Author: Nolan Young Zabala
# Description: - load EDGAR emissions grid data
#              - merge with GADM grids
#              - compute heatmap vars - e.g. average grid totals 2017-22
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(dplyr)
library(ncdf4)
library(sf)
library(ggplot2)

# Set working directory (nc files are saved in my Turkiye directory) - ADJUST FILE PATH HERE
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-turkiye")


#-------------------------------- 2. LOAD DATA ---------------------------------

# EDGAR Totals
total22 <- nc_open("raw_data/edgar/GHG_Totals_2022.nc")

# ------
# EDGAR Sectors - following Crippa et al grouping

# Power: "Power industry"
power <- nc_open("raw_data/edgar/GHG22_PowerIndustry.nc")

# Industrial Combustion & Processes: "Combustion for manufacturing", "Non-metallic minerals production",
#                                    "Chemical Processes", "Iron and Steel Production", "Non-ferrous metals
#                                    production", "Solvents and product use", 
chemicals <- nc_open("raw_data/edgar/GHG22_ChemicalProcesses.nc")
manufac <- nc_open("raw_data/edgar/GHG22_CombustionForManufac.nc")
ironsteel <- nc_open("raw_data/edgar/GHG22_IronSteel.nc")
nonferrous <- nc_open("raw_data/edgar/GHG22_NonFerrousMetals.nc")
nonmetalminerals <- nc_open("raw_data/edgar/GHG22_NonMetalMinerals.nc")
solvents <- nc_open("raw_data/edgar/GHG22_Solvents.nc")

# Transport: "Aviation climbing&descent", "Aviation cruise", "Aviation landing&takeoff", 
#            "Road transportation", "Railways, pipelines, off-road transport",
#            "Shipping"
aviationclimb <- nc_open("raw_data/edgar/GHG22_AviationClimb.nc")
aviationcruise <- nc_open("raw_data/edgar/GHG22_AviationCruise.nc")
aviationlanding <- nc_open("raw_data/edgar/GHG22_AviationLanding.nc")
railway <- nc_open("raw_data/edgar/GHG22_Railway.nc")
road <- nc_open("raw_data/edgar/GHG22_RoadTransport.nc")
shipping <- nc_open("raw_data/edgar/GHG22_Shipping.nc")

# Agriculture:
agriN2O <- nc_open("raw_data/edgar/GHG22_AgriN2O.nc")
agrisoils <- nc_open("raw_data/edgar/GHG22_AgriSoils.nc")
agriwaste <- nc_open("raw_data/edgar/GHG22_AgriWaste.nc")
enteric <- nc_open("raw_data/edgar/GHG22_EntericFermentation.nc")
indirect <- nc_open("raw_data/edgar/GHG22_IndirectEmissionsNOX.nc")
manure <- nc_open("raw_data/edgar/GHG22_Manure.nc")

# Fuel exploitation: "Fuel exploitation", "Oil refineries & Transformation industry"
fuel <- nc_open("raw_data/edgar/GHG22_FuelExploitation.nc")
oil <- nc_open("raw_data/edgar/GHG22_Oil.nc")

# Waste:
solidincineration <- nc_open("raw_data/edgar/GHG22_SolidWasteIncineration.nc")
solidlandfills <- nc_open("raw_data/edgar/GHG22_SolidWasteLandfills.nc")
wastewater <- nc_open("raw_data/edgar/GHG22_WasteWaterHandling.nc")

# Buildings: "Energy for buildings"
buildings <- nc_open("raw_data/edgar/GHG22_BuildingsEnergy.nc")
# ------

# GADM
gadm <- read_sf("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh/raw_data/shapefiles/gadm41_BGD_2.shp") %>% 
  select(geometry)


#------------------------ 3. EXTRACT VARS FROM NC ------------------------------

edgar_clean <- function(edgar){
  
  # Coordinates
  lat <- ncvar_get(edgar, "lat")
  lon <- ncvar_get(edgar, "lon")
  
  # GHG emissions + fill NAs
  emissions_array <- ncvar_get(edgar, "emissions") 
  
  fillvalue <- ncatt_get(edgar, "emissions", "_FillValue")
  
  emissions_array[emissions_array==fillvalue$value] <- NA
  
  # Convert to dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  
  emissions_vec_long <- as.vector(emissions_array)
  
  emissions_obs <- data.frame(cbind(lonlat, emissions_vec_long))
  
  emissions_obs <- emissions_obs %>% 
    rename(Lon = Var1) %>% 
    rename(Lat = Var2) %>% 
    rename(Emissions = emissions_vec_long)
  
  return(emissions_obs)
}

# Totals
total22 <- edgar_clean(total22)

total22 <- total22 %>% 
  rename(EmissionsTotal = Emissions) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

# Power
power <- edgar_clean(power)

power_sector <- power %>% 
  rename(EmissionsTotal = Emissions) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(power)

# Industrial Combustion & Processes
chemicals <- edgar_clean(chemicals)
manufac <- edgar_clean(manufac)
ironsteel <- edgar_clean(ironsteel)
nonferrous <- edgar_clean(nonferrous)
nonmetalminerals <- edgar_clean(nonmetalminerals)
solvents <- edgar_clean(solvents)

industrial_sector <- left_join(chemicals, manufac, by = c("Lon", "Lat")) %>% 
  left_join(ironsteel, by = c("Lon", "Lat")) %>% 
  left_join(nonferrous, by = c("Lon", "Lat")) %>% 
  left_join(nonmetalminerals, by = c("Lon", "Lat")) %>% 
  left_join(solvents, by = c("Lon", "Lat")) %>% 
  mutate(EmissionsTotal = rowSums(select(., starts_with("Emissions")))) %>% 
  select(Lon, Lat, EmissionsTotal) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(chemicals, manufac, ironsteel, nonferrous, nonmetalminerals, solvents)

# Transport
aviationclimb <- edgar_clean(aviationclimb)
aviationcruise <- edgar_clean(aviationcruise)
aviationlanding <- edgar_clean(aviationlanding)
railway <- edgar_clean(railway)
road <- edgar_clean(road)
shipping <- edgar_clean(shipping)

transport_sector <- left_join(aviationclimb, aviationcruise, by = c("Lon", "Lat")) %>% 
  left_join(aviationlanding, by = c("Lon", "Lat")) %>% 
  left_join(railway, by = c("Lon", "Lat")) %>% 
  left_join(road, by = c("Lon", "Lat")) %>% 
  left_join(shipping, by = c("Lon", "Lat")) %>% 
  mutate(EmissionsTotal = rowSums(select(., starts_with("Emissions")))) %>% 
  select(Lon, Lat, EmissionsTotal) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(aviationclimb, aviationcruise, aviationlanding, railway, road, shipping)


# Agriculture
agriN2O <- edgar_clean(agriN2O)
agrisoils <- edgar_clean(agrisoils)
agriwaste <- edgar_clean(agriwaste)
enteric <- edgar_clean(enteric)
indirect <- edgar_clean(indirect)
manure <- edgar_clean(manure)

agri_sector <- left_join(agriN2O, agrisoils, by = c("Lon", "Lat")) %>% 
  left_join(agriwaste, by = c("Lon", "Lat")) %>% 
  left_join(enteric, by = c("Lon", "Lat")) %>% 
  left_join(indirect, by = c("Lon", "Lat")) %>% 
  left_join(manure, by = c("Lon", "Lat")) %>% 
  mutate(EmissionsTotal = rowSums(select(., starts_with("Emissions")))) %>% 
  select(Lon, Lat, EmissionsTotal) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(agriN2O, agrisoils, agriwaste, enteric, indirect, manure)


# Fuel exploitation
fuel <- edgar_clean(fuel)
oil <- edgar_clean(oil)

fuelexploitation_sector <- left_join(fuel, oil, by = c("Lon", "Lat")) %>% 
  mutate(EmissionsTotal = rowSums(select(., starts_with("Emissions")))) %>% 
  select(Lon, Lat, EmissionsTotal) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(fuel, oil)

# Waste
solidincineration <- edgar_clean(solidincineration)
solidlandfills <- edgar_clean(solidlandfills)
wastewater <- edgar_clean(wastewater)

waste_sector <- left_join(solidincineration, solidlandfills, by = c("Lon", "Lat")) %>% 
  left_join(wastewater, by = c("Lon", "Lat")) %>% 
  mutate(EmissionsTotal = rowSums(select(., starts_with("Emissions")))) %>% 
  select(Lon, Lat, EmissionsTotal) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(solidincineration, solidlandfills, wastewater)

# Buildings
buildings <- edgar_clean(buildings)

buildings_sector <- buildings %>% 
  rename(EmissionsTotal = Emissions) %>% 
  filter(between(Lon, 80, 95)) %>% 
  filter(between(Lat, 19, 30))

rm(buildings)

#-------------------------- 4. ASSIGN TO GRIDS --------------------------------

assign_grids <- function(edgar){
  
  # Convert emissions dataframe to sf object
  sf_emissions <- st_as_sf(edgar, 
                           coords = c("Lon", "Lat"), 
                           crs = st_crs(gadm)) 
  
  # Assign firms to grids
  sf_emissions_joined <- st_join(gadm,
                                 sf_emissions,
                                 left = TRUE)
  
  # Sum emissions in each grid for each year and then take average
  final <- sf_emissions_joined %>% 
    group_by(geometry) %>% 
    summarise(HeatVar = sum(EmissionsTotal)) %>% 
    mutate(HeatVar = HeatVar/1000000) %>%
    mutate(HeatVar = ifelse(HeatVar == 0, NA, HeatVar))
  
  return(final)
}


# Totals
total22 <- assign_grids(total22)

# Power
power_sector <- assign_grids(power_sector)

# Industrial Combustion & Processes
industrial_sector <- assign_grids(industrial_sector)

# Agriculture
agri_sector <- assign_grids(agri_sector)

# Transport
transport_sector <- assign_grids(transport_sector)

# Waste
waste_sector <- assign_grids(waste_sector)

# Buildings
buildings_sector <- assign_grids(buildings_sector)


#-------------------------- 5. HEAT MAPS ---------------------------------------

heatplot <- function(df, plot_title, legend_title){   
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient(low = "lightgrey", high = "red") +     
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}

heatplot(total22, "Total GHG Emissions 2022", "Tons (millions)")

heatplot(industrial_sector, "Industrial Sector GHG Emissions 2022", "Tons (millions)")
