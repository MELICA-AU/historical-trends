# LoadData.R
library(tidyverse)
library(sf)
library(dplyr)
library(mapview)

# ==== Year parameter ====
# Choose either 1952 or 1986 to get status in 1951 (post-war) or 1985 (nuclear re-awakening)
year_filter <- 1952  

### ------------- Aarhus City Boundary
ifelse(year_filter == 1952, 
city <- read_sf("../data/Topo/AngelDigitizedAarhusBorders/Poly1952.shp") %>% 
  st_transform(crs = 25832), 
# city <- read_sf("../data/Topo/Borders/Poly1956.shp")
  city <- read_sf("../data/Topo/AngelDigitizedAarhusBorders/Poly1988.shp"))


### ------------- Shelters ---

# ---- BDG
BDG <- readRDS("../../shelter-data/output_data/BDG_long.rds") %>%
  dplyr::select(shelterID = BDnr, Seats = Capacity, Year = Location_startdate) %>%
  filter(Year < year_filter) %>% 
  group_by(shelterID) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  ungroup()

glimpse(BDG)

# ---- KOB
KOB <- readRDS("../../shelter-data/output_data/KOB.rds") %>%
  dplyr::select(shelterID = Number, Seats = Capacity, Year) %>%
  filter(Year < year_filter)

# ---- SR
SR <- read_sf("../../shelter-data/output_data/SR.geojson")%>%
  dplyr::select(shelterID = ID, Seats = places, Year = year) %>% 
  st_transform(4326) 


SR <- if (year_filter < 1960) {
  SR %>% 
    filter(Year < year_filter, Year > 1948)
} else {
  SR %>%
    filter(Year < year_filter)
}

# ---- SOB
SOB <- readRDS("../../shelter-data/output_data/SOB.rds") %>% 
  dplyr::select(shelter_num, Seats = capacity_max, Year = year_of_planned_construction) %>% 
  mutate(shelterID = paste0("SOB-",shelter_num)) %>% 
  dplyr::select(-shelter_num)


#mapview(SR, cex = "Seats", zcol = "Year") + mapview(aarhus_city1952, col.regions = "white", alpha = 0.3) 
#mapview(BDG, cex = "Capacity", zcol = "decade")

# ==== Combine shelter datasets ====

shelters <- rbind(BDG, KOB, SR)

shelters <- shelters %>% 
   mutate(
    shelter_type = case_when(
      grepl("^[0-9]+$", shelterID) & as.numeric(shelterID) %in% 1:36 ~ "KOB",
      grepl("^[0-9]+$", shelterID) & as.numeric(shelterID) >= 100    ~ "BDG",
      grepl("[^0-9]", shelterID)                                     ~ "SR",
      TRUE                                                           ~ NA_character_
    )) 

# any duplicates?
shelters %>% 
  filter(duplicated(shelterID)) 

# get only those within the city border
shelters <- shelters %>% 
  filter(lengths(st_intersects(geometry, st_transform(city,4326))) > 0)

  #st_filter(st_transform(city,4326), join = "st_intersects") 


mapview(shelters)

### ------------- Prep Buildings

bbr_aarhus_data <- readRDS("../data/bbr_all_aarhus.rds") # melica-au file system

buildings <- bbr_aarhus_data %>% 
  filter(byg026Year <1952) %>% 
  rename(CodeNum = byg021BygningensAnvendelse) %>% 
  mutate(Purpose_7groups = case_when(
    # 1) Housing (permanent residential)
    CodeNum %in% c(110,120,121,122,130,131,132,140,150,160,185,190) ~
      "Housing (permanent residential)",
    
    # 2) Production & logistics (industry, agriculture, port)
    CodeNum %in% c(210:219, 221,222,223,229, 290, 315) ~
      "Harbour, workshops and production",
    
    # 3) Commerce & private services
    CodeNum %in% c(321,322,323,324,325,329, 331,332,333,334,339) ~
      "Commerce & private services",
    
    # 4) Public services & administration
    CodeNum %in% c(442,443,444,449,451) ~
      "Public administration & services",
    
    # 5) Education, health & care
    CodeNum %in% c(421,422,429, 431,432,433,439, 441) ~
      "Education and health care",
    
    # 6) Culture, religion & leisure
    CodeNum %in% c(411,412,413,414,415,416,419,
                   510,521,522,523,529,
                   531,532,533,534,535,539,
                   540,585,590) ~
      "Church, culture and leisure",
    
    # 7) Infrastructure, transport & utilities (incl. ancillary/outbuildings)
    CodeNum %in% c(231,232,233,234,239,
                   311,312,313,314,319,
                   910,920,930,940,950,960,970,
                   990,999) ~
      "Infrastructure, transport & utilities",
    
    # If you still have UDFASES codes in the data:
    CodeNum %in% c(210,220,230,310,320,330,410,420,430,440,490,520,530,390) ~
      "Other/legacy code (UDFASES)",
    TRUE ~ "Unknown / check")) 

table(buildings$Purpose_7groups)


### -------------  Prep additional map components

library(geodata)

#
###-------------  Danish border
#dk <- gadm(country = "DNK", level = 0, path = "data/")

dk <- read_rds("../data/gadm/gadm41_DNK_0_pk.rds") %>% 
  terra::unwrap() 
dk32 <- dk %>% 
  st_as_sf() %>% 
 st_transform(25832)

# Create a bounding box for Aarhus if you don't have a shapefile
# aarhus_bbox_sm <- st_as_sfc(st_bbox(c(xmin = 10.1, ymin = 56.1, xmax = 10.25, ymax = 56.2), crs = st_crs(4326)))
# aarhus_bbox_m <- st_as_sfc(st_bbox(c(xmin = 10.05, ymin = 56.05, xmax = 10.28, ymax = 56.25), crs = st_crs(4326)))
# aarhus_bbox <- st_as_sfc(st_bbox(c(xmin = 9.99, ymin = 56.0, xmax = 10.35, ymax = 56.3), crs = st_crs(4326)))
# mapview(aarhus_bbox_m)

# Skip Bornholm
dk_bbox <- st_as_sfc(st_bbox(c(xmin = 8.07, ymin = 54.5, xmax = 13, ymax = 58), 
                             crs = st_crs(4326)))



### ------------- Prep Roads & Communications

roads <- readRDS("../../accessibility/data/streets_aarhus_mun.geojson") %>% 
  st_transform(crs = 25832) %>% 
  st_filter(city, .predicate = st_intersects)

# 1) A lean "map core" dataset
roads_map <- roads %>%
  select(
    osm_id, name, ref, highway,
    oneway, bridge, tunnel, layer,
    lanes, maxspeed, surface, lit,
    distance_m, bike_speed, walk_speed, time,
    geometry
  )

(roads_map)


rail <- read_sf("../data/jernbane_historisk_v05042013/jernbane_historisk.shp") %>% 
   st_filter( st_transform(aarhus_bbox,crs = 25832), .predicate = st_intersects) 
unique(rail$closed)  

# rail <- read_sf("../data/DNK_railway.gpkg") %>% 
#   st_filter(aarhus_bbox, .predicate = st_intersects) %>% 
#   st_transform(crs = 25832) 


###------------- Historical maps

# hist_map <- rast("C:/Users/Adela/Downloads/1952_1_4326.tif")
# hist_map <- terra::aggregate(hist_map, fact = 10, fun = "modal")

# mapview(aa_1988) + mapview(city, col.regions = "orange")+ mapview(aa_1956, col.regions = "pink")


###------------- Quick Mapview

buildings %>% st_filter(city, .predicate = st_intersects) %>% 
  mapview(cex = 0.5, color = "lightgrey")+
  mapview(city, col.regions = "white", alpha = 0.5) + 
  mapview(roads, color = "darkgrey", cex = 0.05)
mapview(shelters, cex = "Seats", zcol = "shelter_type")


# Optional parks layer
# parks <- st_read("data/parks.shp", quiet = TRUE)

### --- Ensure common CRS ---

dk32 <- dk %>% 
  st_transform(crs = 25832)


# parks <- st_transform(parks, st_crs(city))



# --- Floors for building symbol sizes ---
bld <- buildings %>%
  st_filter(city, .predicate = st_intersects) %>% 
  mutate(
    floors = suppressWarnings(as.numeric(byg054AntalEtager)),
    floors = ifelse(is.na(floors) | floors < 1, 1, floors),
    floors = ifelse(floors > 5, 5, floors) # cap odd outliers
  )

# --- Roads: ensure a clean 2-class factor (Main/Secondary) ---
roads <- roads_map %>%
  mutate(
    road_type = case_when(
      highway %in% c("motorway","motorway_link","trunk","trunk_link","primary","primary_link") ~ "Main road",
      highway %in% c("secondary","secondary_link", "residential","service","platform") ~ "Secondary road",
      highway %in% c("footway", "path", "unclassified", "tertiary_link", "tertiary", "cycleway", "steps", "pedestrian", "living_street") ~ "Tertiary road",
      TRUE ~ "Tertiary road"  # force even NAs into a class
    )
  ) %>%
  filter(!is.na(road_type)) %>%
  mutate(road_type = factor(road_type, levels = c("Main road","Secondary road", "Tertiary road")))

roads %>%
  st_drop_geometry() %>%
  count(road_type, sort = TRUE)
