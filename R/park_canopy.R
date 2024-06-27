park_canopy <- function(canopy, points, parks){
  
  # Canopy Cover ------------------------------------------------------------
  # use 2021 LiDAR data from CMM to recalculate canopy cover of each plot + park
  cancov <- read_stars(canopy)
  # sampling points from GPS
  pts <- read_sf(points)
  # park polygons
  parks <- read_sf(parks)
  
  # convert to same projection as canopy cover 
  pts_trans <- st_transform(pts, st_crs(cancov))
  parks_trans <- st_transform(parks, st_crs(cancov))
  
  # select forested past land use 
  pts_for <- pts_trans[grepl("FOR", pts_trans[["Name"]]), ]
  parks_for <- parks_trans[parks_trans$PastLandUse == "Forested", ]
  
  # create 20 m square buffers around sampling points representing plots 
  plots <- st_buffer(pts_for, dist = 20, nQuadSegs=1, endCapStyle = "SQUARE")
  
  parks_can <- aggregate(cancov, parks_for, FUN = function(x) sum(x == 4)/length(x)) %>% 
    st_as_sf() %>% 
    rename(percan = `660_IndiceCanopee_2021.tif`) %>%
    mutate(Park = parks_for$Name)
  
  return(parks_can)
  
}