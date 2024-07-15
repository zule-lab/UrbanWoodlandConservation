make_figure_1 <- function(parks_spatial){

  studyparks <- read_sf(parks_spatial)
  
  # set up bounding box - order: xmin, ymin, xmax, ymax
  bb <- c(xmin = -74.0788,
          ymin = 45.3414,
          xmax = -73.3894,
          ymax = 45.7224)
  
  ## Montreal
  # Download island boundary in bbox
  mtl <- opq(bb) %>%
    add_osm_feature(key = 'place', value = 'island') %>%
    osmdata_sf()
  # Grab multipolygons (large islands)
  multipolys <- mtl$osm_multipolygons
  # Grab polygons (small islands)
  polys <- mtl$osm_polygons
  polys <- st_cast(polys, "MULTIPOLYGON")
  # Combine geometries and cast as sf
  mtl <- st_as_sf(st_union(polys, multipolys))
  
  ## Water
  water <- opq(bb) %>%
    add_osm_feature(key = 'natural', value = 'water') %>%
    osmdata_sf()
  mpols <- water$osm_multipolygons
  mpols <- st_cast(mpols, "MULTIPOLYGON")
  water <- st_as_sf(st_make_valid(mpols))
  
  
  # CRS
  mtlcrs <- st_crs(3347)
  mtl <- st_transform(mtl, mtlcrs)
  studyparks <- st_transform(studyparks, mtlcrs)
  water <- st_transform(water, mtlcrs)
  
  
  # Forested Parks ----------------------------------------------------------
  # select forested parks 
  forparks <- studyparks[studyparks$PastLandUse == "Forested", ]
  
  # calculate centroids of polygons for labels 
  cen <- st_centroid(forparks)
  cen <- st_transform(cen, "+proj=omerc +lat_0=45.65 +lonc=-73.80 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=40")
  
  # assign as Wooded Urban Parks vs Conservation Parks
  wup <- c('Jean-Drapeau', 'Angrignon', 'Thomas-Chapais', 'Coulée Grou', 'Boisé-du-Saint-Sulpice')
  
  forparks <- forparks %>% 
    mutate(Status = case_when(
      Name %in% wup ~ 'Wooded Urban Park',
      TRUE ~ 'Conservation Park'),
      lon = st_coordinates(cen)[,1],
      lat = st_coordinates(cen)[,2])
  
  
  
  
  # Theme -------------------------------------------------------------------

  ## inset 
  thememtl <- theme(panel.border = element_rect(linewidth = 1, fill = NA),
                    panel.background = element_rect(fill = '#ddc48d', colour = "#e7e5cc"),
                    panel.grid = element_line(color = '#73776F', linewidth = 0.2),
                    axis.text = element_text(size = 11, color = 'black'),
                    axis.title = element_blank(), 
                    plot.background = element_blank(),
                    legend.position = "top",
                    legend.text = element_text(size = 12),
                    legend.background = element_rect(fill = NA)
  )
  
  
  # Map ---------------------------------------------------------------------
  
  bbi <- st_bbox(mtl)
  crs_string = "+proj=omerc +lat_0=45.65 +lonc=-73.80 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=40"
  
  p <- ggplot() +
    geom_sf(fill = '#ceb99b', data = mtl) + 
    geom_sf(aes(fill = Status), data = forparks) +
    scale_fill_manual(values = c("#2C4B27", "#7FA074"), labels=c('Nature Park', 'Non-Status Urban Woodland')) +
    geom_sf(fill = '#99acc3', data = water) + 
    geom_label_repel(aes(x = lon, y = lat, label = Name, fill = Status), colour = "white", fontface = "bold", show.legend = FALSE, size = 4, alpha = 0.8, data = forparks) + 
    coord_sf(crs = crs_string, 
             xlim = c(-17500, 15000),
             ylim = c(-20000, -4000)) +
    labs(colour = "") +
    thememtl
  
  ggsave("figures/fig-1.png", p, width = 10, height = 8, units = "in", dpi = 450)
  
  
  
}