###  Basic spatial data preparation


library(sf)
library(mapview)
library(tidyverse)

#install.packages("geodata")
library(geodata)

########### Get Denmark and Aarhus municipal boundary

dk <- gadm(country = "DNK", level = 2, path = "data/")
aa_mun <- dk %>% st_as_sf() %>% filter(NAME_2 == "Århus")
plot(aa_mun)
st_write(aa_mun, "data/aarhus_mun.geojson")

############### Get World Cities

# https://github.com/ok-dk/dagi/tree/master/geojson
wcities <- st_read("data/cities.geojson")
A_city <- wcities[wcities$NAME == "ARHUS",]

mapview(A_city)

########### Aarhus bboxes: 

# Create a bounding box for Aarhus if you don't have a shapefile
aarhus_bbox_sm <- st_as_sfc(st_bbox(c(xmin = 10.1, ymin = 56.1, xmax = 10.25, ymax = 56.2), crs = st_crs(4326)))
aarhus_bbox_m <- st_as_sfc(st_bbox(c(xmin = 10.05, ymin = 56.05, xmax = 10.28, ymax = 56.25), crs = st_crs(4326)))
aarhus_bbox <- st_as_sfc(st_bbox(c(xmin = 9.99, ymin = 56.0, xmax = 10.35, ymax = 56.3), crs = st_crs(4326)))

dk <- dk %>% 
  st_as_sf() 

# Skip Bornholm
dk_bbox <- st_as_sfc(st_bbox(c(xmin = 8.07, ymin = 54.5, xmax = 13, ymax = 58), crs = st_crs(4326)))
mapview(dk_bbox)


############## Aarhus "trading city" boundary
# Data from Claus

# A <- st_read("data/Koebstadskommune.geojson")%>%
#   st_as_sf() %>%
#   st_make_valid()
# 
# unique(A$navn)
# A %>%
#   filter(navn =="Århus Købstadskommune") %>%
#   #filter(fra == "1908-07-01") %>%
#   mapview(zcol = "fra", burst = TRUE)
# 
# A %>%
#   filter(navn =="Århus Købstadskommune") %>%
#   st_write("data/Aarhus_koebstkom_pre1970.geojson")


A_koeb <- st_read("data/Aarhus_koebstkom_pre1970.geojson")
library(lubridate)
A_koeb %>%
  select(fra, fid) %>% 
  arrange(fra) %>% 
  mutate(date = as_date(fra)) %>% glimpse()
  
A_koeb %>% 
  filter(fra > "1890-01-01") %>% 
  mapview(zcol = "fra", burst = TRUE, alpha = 0.5)

######### Aarhus parish/church boundaries
# A_sogne <- st_read("data/Sognekommune.geojson")%>%
#   st_as_sf() %>%
#   mutate(geometry = st_make_valid(geometry))
# 
# # Identify invalid geometries
# invalid_polygons <- A_sogne %>%
#   filter(!st_is_valid(geometry))
# 
# unique(A_sogne$navn)
# 
# # Function to safely run st_intersects
# safe_intersects <- function(geom, bbox) {
#   tryCatch({
#     st_intersects(geom, bbox, sparse = FALSE)
#   }, error = function(e) {
#     FALSE  # If an error occurs, treat it as no intersection
#   })
# }
# 
# # Apply the filtering
# overlapping_sogne <- A_sogne %>%
#   filter(sapply(geometry, safe_intersects, bbox = aarhus_bbox_m))  # Apply safe_intersects on each geometry
# 
# # View the result
# overlapping_sogne %>%
#   distinct(geometry) %>%
#  # filter()
#   mapview()
# mapview(overlapping_sogne, zcol = "fra", burst = TRUE)
# 
# overlapping_polygons %>%
# #   st_write("data/Aarhus_sogne_pre1970.geojson")

## Defining "urban" area of Aarhus
# https://www.dst.dk/en/Statistik/dokumentation/documentationofstatistics/urban-areas/statistical-presentation


A_sogne <- st_read("data/Aarhus_sogne_pre1970.geojson")
A_sogne %>% 
  filter(fra < "1900-01-01") %>% 
  mapview( zcol = "fra", burst = TRUE) + 
  mapview(A_koeb %>% filter(fid == 321))


glimpse(A_koeb)
A_koeb %>% filter(fra == 1838-07-02)
## Defining "urban" area of Aarhus
# https://www.dst.dk/en/Statistik/dokumentation/documentationofstatistics/urban-areas/statistical-presentation
