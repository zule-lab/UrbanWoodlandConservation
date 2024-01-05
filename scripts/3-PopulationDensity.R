# Packages ----------------------------------------------------------------
p <- c("stringr", "dplyr", "tidyr", "tibble", "ggplot2", "iNEXT", "sf", "stars")
lapply(p, library, character.only = T)


# Population Density ------------------------------------------------------
# read population data from 2021 census 
census <- read.csv("input/quebec_census_2021/98-401-X2021006_English_CSV_data_Quebec.csv")
da_bounds <- read_sf("input/quebec_census_2021/lda_000b21a_e.shp")
parks <- read_sf("output/study_parks_spatial.gpkg")

# select population 2021 data 
census <- census[census$CHARACTERISTIC_NAME == "Population, 2021" ,]
# select dissemination area geography level 
da <- census[census$GEO_LEVEL == "Dissemination area", ]

# associate population density with spatial dissemination areas 
pop_spatial <- left_join(da, da_bounds, by = "DGUID") %>% 
  mutate(density = C1_COUNT_TOTAL/LANDAREA) %>% # calculate population density (population / land area (km2))
  st_as_sf()

# calculate 400 m (5 min walking distance) buffer around parks 
parks_trans <- st_transform(parks, st_crs(da_bounds))
parks_for <- parks_trans[parks_trans$PastLandUse == "Forested", ]
parks_buff <- st_buffer(parks_for, dist = 400)

# intersect park buffers with dissemination areas 
parks_da <- st_intersection(parks_buff, pop_spatial)

# calculate mean population density / km2 surrounding park
parks_da_dens <- parks_da %>%
  group_by(Name) %>% 
  summarize(totalpop = sum(C1_COUNT_TOTAL),
            totalarea = sum(LANDAREA),
            avgdens = totalpop/totalarea)

write_sf(parks_da_dens, "output/park_pop_density.gpkg")