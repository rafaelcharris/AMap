# Load required libraries
rm(list = ls())
library(tidyverse)
library(ggmap)
library(tigris)
library(janitor)
library(osmdata)
library(sf)
library(emojifont)

target_crs <- 4326  # Or 4326 for lat/lon

# 1.  Read the CSV file
locations <- read_csv("data/nyc_locations_extended.csv")

#2. download geocoded 

#geocoded_loc <- read_csv("data/nyc_locations_extended_geocoded.csv")
#set_overpass_url("https://overpass.kumi.systems/api/interpreter")
#
##3. parks 
#jc_parks <- opq("Jersey City, New Jersey") %>%
#  add_osm_feature(key = "leisure", value = "park") %>%
#  osmdata_sf()

#3. parks 
set_overpass_url("https://lz4.overpass-api.de/api/interpreter")
nj_parks <- opq(bbox = c(-74.1, 40.7, -73.9, 40.8)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

# Extract polygon geometries
parks_poly <- nj_parks$osm_polygons

parks <- read_csv("data/Parks_Properties_20250728.csv") %>%
  clean_names() %>%
  filter(class == "PARK") %>%
  mutate(geometry = st_as_sfc(multipolygon)) %>%
  st_as_sf(crs = target_crs)

# Avery's places 
av_places <- read_csv("data/nyc_locations_extended_geocoded.csv")
av_places_l <- locations %>% 
  bind_cols(av_places) %>%
  mutate(label = case_when(
        category == "AUDIO SPICE ACTION" ~  emoji('microphone'), 
        category == "PRIVATE ACTION" ~ emoji('star'),
        category == "ACADEMIC/HUMAN ACTION" ~ emoji('classical_building'), 
        TRUE ~ '')) 

av_places_point <- st_as_sf(av_places, coords = c("Longitude", "Latitude"),  crs = 4326)

av_all <- locations %>%
  bind_cols(av_places_point)

bbox <- st_bbox(av_places_point)

# get roads 
roads_data = primary_secondary_roads("NY")
bk_roads <- tigris::roads(state ="NY", county = "Kings")
queens_roads <- tigris::roads(state ="NY", county = "Queens")
#si_roads <- tigris::roads(state ="NY", county = "Richmond")
nyc_roads <- tigris::roads(state ="NY", county = "New York")
bronx_roads <- tigris::roads(state ="NY", county = "Bronx")

nyc_roads <- bind_rows(bk_roads, nyc_roads, bronx_roads, 
                       queens_roads)
## NJ
nj_roads_data = primary_secondary_roads("NJ")
hudson_roads <- tigris::roads(state ="NJ", county = "Hudson")
bergen_roads <- tigris::roads(state ="NJ", county = "Bergen")

nj_counties_roads <- bind_rows(
  hudson_roads,
 bergen_roads)

# get water
ny_water = area_water("NY", "New York")
bk_water = area_water("NY", "Kings")
q_water = area_water("NY", "Queens")
si_water = area_water("NY", "Richmond")
brx_water = area_water("NY", "Bronx")

hudson_water = area_water("NJ", "Hudson")
bergen_water = area_water("NJ", "Bergen")

nyc_water = bind_rows(ny_water,
                      bk_water, q_water,
                      si_water, brx_water)
nyc_water_dis <- nyc_water %>%
  st_union()

nj_water = bind_rows(hudson_water,
                      bergen_water) 

nj_water_dis <- nj_water %>%
  st_union()
  
# Get the base map of NYC
ny_counties = counties(state = "NY")
nj_counties = counties(state = "NJ")

nyc = ny_counties %>%
  clean_names() %>%
  # including Staten Island
  filter(grepl("Kings|Bronx|Queens|York|Richmond", name))
  # Not including Staten Island
  #filter(grepl("Kings|Bronx|Queens|York", name))

nj = nj_counties %>%
  clean_names() %>%
 filter(grepl("Hudson|bergen", name, ignore.case = TRUE)) 

limits = st_bbox(nyc$geometry)

# Get all in the same crs
nyc <- st_transform(nyc, target_crs)
nyc_water <- st_transform(nyc_water, target_crs)
roads_data <- st_transform(roads_data, target_crs)
parks_tr<- st_transform(parks, target_crs)

# Get parks 


## Connect dots
av_places_s <- av_places %>% 
  rowid_to_column() 

av_places_s <- av_places %>%
  select(rowid, Longitude, Latitude)

home <- av_places_s[1,c(2,3)]

destinations <- av_places_s[c(2:23),c(1, 2,3)]

# Get routes from home to each destination
routes <- lapply(1:nrow(destinations), function(i) {
  osrmRoute(src = home,
            dst = destinations[i, c(2,3)],
            overview = "full",
            osrm.profile = "foot",   # or "foot"
            returnclass = "sf") %>%
    mutate(dest_id = destinations$rowid[i])
})
routes_sf <- do.call(rbind, routes)

# Plot map =====
av_map <- ggplot(nyc) + 
  geom_sf(col = "black", linetype = 5) +
  geom_sf(data = nj, aes(geometry = geometry), linewidth = 0) + 
  geom_sf(data = parks, aes(geometry = geometry), fill = "#bae4b3") + 
  geom_sf(data = parks_poly, aes(geometry = geometry), fill = "#bae4b3") + 
  # Include Water
  geom_sf(data = nyc_water_dis,
          aes(geometry = geometry), fill = '#cce6ff') + 
  geom_sf(data = roads_data, aes(geometry = geometry), col = "white") +
  geom_sf(data = nyc_roads, 
          aes(geometry = geometry), col = "white", linewidth = 0.1
          ) + 
  geom_sf(data = nj_water_dis, aes(geometry = geometry), 
          fill = '#cce6ff') +
  geom_sf(data = nj_roads_data, aes(geometry = geometry), col = "white") +
  geom_sf(data = nj_counties_roads, 
          aes(geometry = geometry), col = "white", linewidth = 0.1
          ) + 
  geom_sf(data = routes_sf, linewidth = 0.4, col = "grey50") + 
  geom_sf(data = av_all, aes(geometry = geometry, col = category),
          size = 1.5) + 
  scale_color_manual(values = c("#556B2F", "#CD3333", "#FFB90F")) + 
  coord_sf(
    xlim = c(-74.06, -73.84),
           ylim = c(as.numeric(limits$ymin) + 0.04, as.numeric(limits$ymax) - 0.02),
           expand = FALSE,
           crs = st_crs(4326)) +
  theme_void() +
  theme(legend.position = "none")

av_map

# Poster
ggsave(av_map, filename = "map_tabloid.png", dpi = 320,
       width = 12, height = 18,
       units = "in")

ggsave(av_map, filename = "map_tabloid.pdf", dpi = 320,
       width = 12, height = 18,
       units = "in")
   

ggsave(av_map, filename = "map_tabloid_v5.svg", dpi = 320,
       width = 7, height = 11,
       units = "in")
  
ggsave(av_map, filename = "map_doulbe_tabloid.svg", dpi = 320,
       width = 16, height = 20,
       units = "in")
  