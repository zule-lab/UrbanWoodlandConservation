# Packages ----------------------------------------------------------------
p <- c("stringr", "dplyr", "tidyr", "tibble", "ggplot2", "iNEXT", "sf", "stars")
lapply(p, library, character.only = T)


# Canopy Cover ------------------------------------------------------------
# use 2021 LiDAR data from CMM to recalculate canopy cover of each plot + park
cancov <- read_stars("input/660_IndiceCanopee_2021.tif")
# sampling points from GPS
pts <- read_sf("input/gps_sppts.gpkg")
# park polygons
parks <- read_sf("input/study_parks_spatial.gpkg")

# convert to same projection as canopy cover 
pts_trans <- st_transform(pts, st_crs(cancov))
parks_trans <- st_transform(parks, st_crs(cancov))

# select forested past land use 
pts_for <- pts_trans[grepl("FOR", pts_trans[["Name"]]), ]
parks_for <- parks_trans[parks_trans$PastLandUse == "Forested", ]

# create 20 m square buffers around sampling points representing plots 
plots <- st_buffer(pts_for, dist = 20, nQuadSegs=1, endCapStyle = "SQUARE")

# calculate canopy cover by dividing number of pixels == 4 (canopy) by total number of pixels within a buffer
pts_can <- aggregate(cancov, plots, FUN = function(x) sum(x == 4)/length(x)) %>% 
  st_as_sf() %>% 
  rename(percan = `660_IndiceCanopee_2021.tif`) %>%
  mutate(Park = plots$Name)

parks_can <- aggregate(cancov, parks_for, FUN = function(x) sum(x == 4)/length(x)) %>% 
  st_as_sf() %>% 
  rename(percan = `660_IndiceCanopee_2021.tif`) %>%
  mutate(Park = parks_for$Name)


# Save --------------------------------------------------------------------

write_sf(pts_can, "output/canopy_points.gpkg")
write_sf(parks_can, "output/canopy_parks.gpkg")
