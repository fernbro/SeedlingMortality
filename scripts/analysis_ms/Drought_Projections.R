library(tidyverse)
library(terra)
library(sf)

dry_current <- rast("data/Climate/Total_Dry_Days_Shallow/DrySoil_shallow_currentconditions.tif")

plot(dry_current)

dry_20_50 <- rast("data/Climate/Total_Dry_Days_Shallow/DrySoil_shallow_d40yrs_RCP45_q50.tif")

plot(dry_20_50)

dry_70_100 <- rast("data/Climate/Total_Dry_Days_Shallow/DrySoil_shallow_d90yrs_RCP45_q50.tif")

plot(dry_70_100)




