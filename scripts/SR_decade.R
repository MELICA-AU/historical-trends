####################################################

### SR data from Ulrik (SR) with sikringsrum

####################################################

# This script creates historical sikringsrum spatial data from SR data provided by Ulrik Nielsen
# It removes empty geometries and renames columns to English, 
# saves data as SR_sikringsrum.geojson
# and creates SR_89 data for historical overview and visualisation
# offers summary mapview and facetted tmaps by decade in undifferentiated dataset

# Libraries 
library(tidyverse)
library(sf)
library(mapview)


###################################################  - SR89 - SUMMARY MAPs with MAPVIEW
# quick map
SR <- st_read("output_data/SR_sikringsrum.geojson")
SR <- SR %>%
  mutate(decade = case_when(
    decade == '1930s' ~ '1180-1939',
    TRUE ~ decade  # Keep the original value for other cases
  )) 
mapview(SR, zcol = "decade")


SR_89 <- SR %>% 
  dplyr::filter(decade < "1990s") 
SR_89 %>% 
  mapview(zcol = "decade")

###################################################  FACETTED MAPs with TMAP
# Visualize sikringsrum construction and capacity over time
library(tmap)
tmap_options(limits = c(facets.view = 8))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(SR)+
  tm_facets(by = "decade",
            ncol = 4)+
  tm_bubbles(size = "places")


# Up to 1989
library(tmap)
tmap_options(limits = c(facets.view = 6))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(SR_89)+
  tm_facets(by = "decade",
            ncol = 6)+
  tm_bubbles(size = "places")

tmap_mode("plot")


##########################################      What next? 
# - reverse geocode
# - intersect with building footprints, crosscheck with archival records, 
# - calculate total capacity per decade and compare with population trends
# - check for duplicates: are all private shelters unique or are some double-entered,
# - because a building got upgraded, etc.?
