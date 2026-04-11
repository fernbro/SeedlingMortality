library(tidyverse)
library(terra)
library(sf)
library(tigris)
library(mapview)

# states:

swus <- states() %>%
  filter(NAME %in% c("New Mexico", "Arizona")) %>% 
  st_as_sf()
st_write(swus, "data/AZ_NM.shp")


# let's extract for the provenance points too:
prov <- st_read("data/Provenance/Prov.shp", crs = "+proj=longlat +datum=WGS84")

prov_bbox <- st_as_sfc(st_bbox(prov))
prov_buffer <- st_buffer(prov_bbox, 10000)

# prov_current <- terra::extract(dry_current, prov, bind = T)
# 
# mapview(terra::crop(dry_2100_rcp85, prov_buffer) - terra::crop(dry_current, prov_buffer)) + mapview(prov_current)


# ecological drought projections:

# 1. length of the longest dry period in shallow soils (days)
    # current period

# only modeled within areas where MAP/PET < 0.6

dry_current <- rast("data/Climate/Longest_Dry_Period_Shallow/DrySoilPeriods_shallow_currentconditions.tif")

    # future periods: 2020-2050, 2070-2100

dry_2050_rcp45 <- rast("data/Climate/Longest_Dry_Period_Shallow/DrySoilPeriods_shallow_d40yrs_RCP45_q50.tif")
dry_2050_rcp85 <- rast("data/Climate/Longest_Dry_Period_Shallow/DrySoilPeriods_shallow_d40yrs_RCP85_q50.tif")

dry_2100_rcp45 <- rast("data/Climate/Longest_Dry_Period_Shallow/DrySoilPeriods_shallow_d90yrs_RCP45_q50.tif")
dry_2100_rcp85 <- rast("data/Climate/Longest_Dry_Period_Shallow/DrySoilPeriods_shallow_d90yrs_RCP85_q50.tif")

# 2. 

# function to create range polygons from basal area rasters:
  make_range_rast <- function(tree){
    tr <- tree
    tr[tr[] < 1] = NA # not in the range if less than 1
    tr[tr[] > 0] = 1 # we are making a binary variable
    tr_sw <- mask(tr, st_transform(swus, st_crs(tr)))
    tr_sw <- crop(tr_sw, st_transform(swus, st_crs(tr_sw)))
    tr_sw <- project(tr_sw, "epsg:4326")
    return(tr_sw)
  }
  
  make_range_poly <- function(tree_binary_rast){
    tr_r <- terra::as.polygons(tree_binary_rast, aggregate = T)
    tr_sf <- st_as_sf(tr_r)
    return(tr_sf)
  }

psme <- rast("../PSME.tif")
psme_range_rast <- make_range_rast(psme)
psme_range <- make_range_poly(psme_range_rast)
# st_write(psme_range, "data/Ranges/PSME_Range.shp")

pipo <- rast("../PIPO.tif")
pipo_range_rast <- make_range_rast(pipo)
pipo_range <- make_range_poly(pipo_range_rast)
# st_write(pipo_range, "data/Ranges/PIPO_Range.shp")
# 
pifl <- rast("../PIFL.tif")
pifl_range_rast <- make_range_rast(pifl)
pifl_range <- make_range_poly(pifl_range_rast)
# st_write(pifl_range, "data/Ranges/PIFL_Range.shp")
# 
pien <- rast("../PIEN.tif")
pien_range_rast <- make_range_rast(pien)
pien_range <- make_range_poly(pien_range_rast)
# st_write(pien_range, "data/Ranges/PIEN_Range.shp")

# psme_range <- st_read("data/Ranges/PSME_Range.shp")
# pipo_range <- st_read("data/Ranges/PIPO_Range.shp")
# pifl_range <- st_read("data/Ranges/PIFL_Range.shp")
# pien_range <- st_read("data/Ranges/PIEN_Range.shp")

# for each spp, time period, and climate scenario:
# spp x current, spp x 40, spp x 90, x 45 x 85

# find % of range that will experience a LD50 drought
# what % of pixels have a value >= the LD50?

# LD50s from a mort_glm <- life ~ days_stressed*spp (no temp included in model)
# visually estimated from ggplot

# PSME 62
# PIPO 75
# PIEN 70
# PIFL 110

# mask each drought projection raster to each species range:
range_drought <- function(range_poly,
                          drought_rast){
  
  x <- mask(drought_rast, st_transform(range_poly, crs(drought_rast))) %>% 
    crop(swus)
  
  return(x)
}

# can take the mean of the drought metric across the range
          # pifl_2100_85 <- range_drought(pifl_range, dry_2100_rcp85)
          # global(pifl_2100_85, fun = mean, na.rm = T)
          # global(range_drought(pifl_range, dry_current), fun = mean, na.rm = T)

# can also convert raster to a binary, representing days of drought above or below our mortality threshold
# then take an average to get the proportion of pixels that = 1
          # global(pifl_2100_85 >= 110, fun = mean, na.rm = T)

# x <- range_drought(pipo_range, dry_2100_rcp85) >= 75

# supp_range <- function(tree_rast, drought_rast){
#   
#   # create a raster that is just the parts of the range 
#   a <- 
#   
# }

# x_range <- mosaic(x, resample(pipo_range_rast, x))

# global(x_range >= 75, fun = mean, na.rm = T)*100

      # REVISIT: # import glm parameters for estimated LD50s:

# range map:
ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pipo_range, color = "#FFC107", fill = "#FFC107")+
  geom_sf(data = psme_range, color = "darkgreen", fill = "darkgreen", alpha = 0.4)+
  geom_sf(data = prov_buffer, fill = NA)+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")
ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pifl_range, color = "#FFC107", fill = "#FFC107")+
  geom_sf(data = pien_range, color = "darkgreen", fill = "darkgreen", alpha = 0.4)+
  geom_sf(data = prov_buffer, fill = NA)+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

mapview(pipo_range)+mapview(psme_range, fill = "red")

# PIPO:
pipo_hist <- range_drought(pipo_range, dry_current) >= 75/2
global(pipo_hist, fun = mean, na.rm = T)*100
pipo_hist_df <- as.data.frame(pipo_hist, xy = T)
names(pipo_hist_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pipo_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = pipo_hist_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

pipo_2100 <- range_drought(pipo_range, dry_2100_rcp85) >= 75/2
global(pipo_2100, fun = mean, na.rm = T)*100
pipo_2100_df <- as.data.frame(pipo_2100, xy = T)
names(pipo_2100_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pipo_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = pipo_2100_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

# PSME:
psme_hist <- range_drought(psme_range, dry_current) >= 62
global(psme_hist, fun = mean, na.rm = T)*100
psme_hist_df <- as.data.frame(psme_hist, xy = T)
names(psme_hist_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = psme_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = psme_hist_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

psme_2100 <- range_drought(psme_range, dry_2100_rcp85) >= 62
global(psme_2100, fun = mean, na.rm = T)*100
psme_2100_df <- as.data.frame(psme_2100, xy = T)
names(psme_2100_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = psme_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = psme_2100_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

# PIEN:
pien_hist <- range_drought(pien_range, dry_current) >= 70
global(pien_hist, fun = mean, na.rm = T)*100
pien_hist_df <- as.data.frame(pien_hist, xy = T)
names(pien_hist_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pien_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = pien_hist_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

pien_2100 <- range_drought(pien_range, dry_2100_rcp85) >= 70
global(pien_2100, fun = mean, na.rm = T)*100
pien_2100_df <- as.data.frame(pien_2100, xy = T)
names(pien_2100_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pien_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = pien_2100_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

# PIFL:
pifl_hist <- range_drought(pifl_range, dry_current) >= 110
global(pifl_hist, fun = mean, na.rm = T)*100
pifl_hist_df <- as.data.frame(pifl_hist, xy = T)
names(pifl_hist_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pifl_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = pifl_hist_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")

pifl_2100 <- range_drought(pifl_range, dry_2100_rcp85) >= 110
global(pifl_2100, fun = mean, na.rm = T)*100
pifl_2100_df <- as.data.frame(pifl_2100, xy = T)
names(pifl_2100_df) <- c("x", "y", "at_risk")

ggplot()+
  geom_sf(data = swus, color = "black", fill = "white")+
  geom_sf(data = pifl_range, color = "#FFC107", fill = "#FFC107")+
  geom_tile(data = pifl_2100_df, aes(x = x, y = y, fill = at_risk))+
  scale_fill_manual(values = c("#E60054", "#1E88E5"))+
  theme_minimal(base_size = 20)+
  labs(x = "", y = "")


# ALTERNATIVE PRESENTATIONS:

# ideas...

# safety margins: subtract DIM LD50 from length of dry period to get the buffer
# in number of days. negative safety margin implies that the dry length is longer 
# than the LD50 threshold; positive values imply that you have more time 
# than would be experienced in the wild.
  # could be choropleth (delineate bands of values) instead of continuous?

# change maps: subtract historical T/F raster (or 1/0) from future, 
# highlighting areas that have shifted in planting suitability.


# change maps:

plot(pipo_hist - pipo_2100)
plot(psme_hist - psme_2100)
hist(psme_hist - psme_2100)
