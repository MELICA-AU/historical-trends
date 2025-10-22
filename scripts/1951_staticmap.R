################--  Static map with an inset and public shelters in 1952!
library(sf)
library(dplyr)
library(tmap)
library(tidyverse)
library(terra)
library(mapview)

year_filter <- 1952  # or any year you want to filter by
source("../spatial-equity/01_LoadData.R")

shelters %>% 
  group_by(shelter_type) %>% 
  tally(Seats)

shelters %>% 
  filter(shelter_type == "BDG") %>% 
  distinct(Seats)

### -------------  Prep additional map components
tm_shape(World, crs = 3035, bb = "Europe") +
  tm_polygons()

#install.packages("geodata")

library(geodata)

dk <- gadm(country = "DNK", level = 0, path = "data/")

# Create a bounding box for Aarhus if you don't have a shapefile
aarhus_bbox_sm <- st_as_sfc(st_bbox(c(xmin = 10.1, ymin = 56.1, xmax = 10.25, ymax = 56.2), crs = st_crs(4326)))
aarhus_bbox_m <- st_as_sfc(st_bbox(c(xmin = 10.05, ymin = 56.05, xmax = 10.28, ymax = 56.25), crs = st_crs(4326)))
aarhus_bbox <- st_as_sfc(st_bbox(c(xmin = 9.99, ymin = 56.0, xmax = 10.35, ymax = 56.3), crs = st_crs(4326)))

dk <- dk %>% 
  st_as_sf() 

# Skip Bornholm
dk_bbox <- st_as_sfc(st_bbox(c(xmin = 8.07, ymin = 54.5, xmax = 13, ymax = 58), crs = st_crs(4326)))
mapview(dk_bbox)

# Aarhus Boundary
aa_1952 <- read_sf("data/Topo/Borders/Poly1952.shp")
aa_1956 <- read_sf("data/Topo/Borders/Poly1956.shp")
aa_1988 <- read_sf("data/Topo/Borders/Poly1988.shp")


# Historical maps
hist_map <- rast("C:/Users/Adela/Downloads/1952_1_4326.tif")
hist_map <- terra::aggregate(hist_map, fact = 10, fun = "modal")

mapview(aa_1988) + mapview(aa_1952, col.regions = "orange")+ mapview(aa_1956, col.regions = "pink")


################-- Quick Mapview

buildings %>% st_filter(aarhus_city1952, .predicate = st_intersects) %>% 
  mapview(cex = 0.5, color = "lightgrey")+
  mapview(aarhus_city1952, col.regions = "white", alpha = 0.5) + 
  mapview(shelters, cex = "Seats", zcol = "shelter_type")

################-- Mapview with historical background

# Reproject if necessary
hist_map_wgs84 <- project(hist_map, "EPSG:4326")

# library(plainview)
# viewRGB()
# Plot in mapview
mapview(hist_map_wgs84, alpha.regions = 0.6) +
  mapview(buildings %>% st_filter(aarhus_city1952, .predicate = st_intersects), color = "lightgrey", cex = 0.5) +
  mapview(shelters, zcol = "shelter_type", cex = "Seats", col.regions = c("orange", "purple"))

### -------------  First test map


tmap_mode(mode = "plot")

main_map <-
  tm_shape(dk, bbox = aa_1988) +
  tm_polygons(fill = "white") +
  
  # tm_shape(SR) +
  # tm_dots(
  #   size = "Seats",
  #   fill_alpha = 0.8,
  #   fill.scale = tm_scale(values = "viridis", label = "Year"),
  #   #size.scale = tm_scale_continuous(label = "Places", values.scale = 1.5),
  #   fill.legend = tm_legend(title = "private shelters", 
  #                           orientation = "portrait")
  # ) +
  # 
  tm_shape(shelters) +
  tm_symbols(
    #fill = "shelter_type",
    col = "black",
    #size = "Seats",
    shape = 22,
    shape = "shelter_type",
    fill_alpha = 0.9,
    fill.scale = tm_scale_categorical(
      values = "brewer.pi-yg",
      labels = c(
        "Public Shelter",
        "Private Shelter"
      )
    ),
    size.scale = tm_scale_continuous(values.scale = 1.3),
    fill.legend = tm_legend(
      title = "Shelter Type",
      text.size = 0.8,
      title.size = 1
    ),
    size.legend = tm_legend(
      title = "Capacity (Seats)",
      text.size = 0.8,
      title.size = 1
    )
  ) +
  
  # city borders over time
  tm_shape(aa_1952) +
  tm_borders(lwd = 1, 
            # lty = "dotted",
             
             col = "darkgrey") +

  tm_title("Public and Private Shelters in Aarhus (1951)") +
  tm_legend(
    position = c("left", "top"),
    title.size = 1.2,
    text.size = 0.8,
    stack = "vertical"
  ) +
  tm_layout(
    frame = TRUE,
    bg.color = "grey85"
  ) +
  tm_compass(type = "8star", position = c("right", "bottom")) +
  tm_scalebar(position = c("right", "bottom"))

main_map
# Create the inset map of Denmark

inset_map <- tm_shape(st_as_sf(dk), bbox = dk_bbox) +
  tm_polygons(col = "gray90", border.col = "white") +  # Denmark base layer in inset
  tm_shape(aarhus_bbox) +
  tm_borders(lwd = 2, col = "red") +  # Bounding box in inset map
  tm_layout(frame = TRUE)  # Remove frame from inset

library(grid)

# Draw the main map
print(main_map)

# Overlay the inset map
vp_inset <- viewport(width = 0.25, height = 0.25, x = 0.15, y = 0.02, just = c("left", "bottom"))

print(inset_map, vp = vp_inset)



############## -------------------------------------  PRINT IT OUT - Figure 

# Step 3: Export the combined map as a TIFF at 400 DPI
tiff("figures/Shelters_map.tiff", width = 7, height = 10, units = "in", res = 400)

# Draw the main map
print(main_map)

# Overlay the inset map
vp_inset <- viewport(width = 0.3, height = 0.3, x = 0.1, y = 0.01, just = c("left", "bottom"))
print(inset_map, vp = vp_inset)

# Close the TIFF device
dev.off()

############## ------------------------------------- BETTER MAP
library(tmap)
library(sf)
library(viridisLite)

tmap_mode("view")

# --- Danish coastline ---
tm_shape(dk, bbox = aa_1988) +
  tm_polygons(fill = "white") +
  
  # --- Historical map ---
  tm_shape(hist_map) +
  tm_rgb(                             # for 3-band (RGB) GeoTIFFs
    col_alpha = 0.8                       # transparency to let vector layers show
  ) +
  
  # --- City border ---
tm_shape(aarhus_city1952) +
  tm_polygons(
   # fill = "white",
    col = "grey60",
    lwd = 1,
    fill_alpha = 0.3
  ) +
  
  # --- Buildings ---
  # tm_shape(buildings) +
  # tm_symbols(
  #   fill = "byg054AntalEtager",
  #   col = "black",
  #   size = "est_pop",
  #   shape = 21,
  #   fill_alpha = 0.9,
  #   fill.scale = tm_scale_continuous(
  #     values = grayscale(10, direction = -1)
  #     #labels = c("1 Floor", "2 Floors", "3 Floors", "4 Floors", "5+ Floors")
  #   ),
  #   size.scale = tm_scale_continuous(values.scale = 1.5),
  #   fill.legend = tm_legend(
  #     title = "Building Height",
  #     text.size = 0.8,
  #     title.size = 1
  #   ),
  #   size.legend = tm_legend(
  #     title = "Estimated Population",
  #     text.size = 0.8,
  #     title.size = 1
  #   )
  # ) +

  # --- Shelters layer (shape by type) ---
  tm_shape(shelters) +
  tm_symbols(
    fill = "Seats",               # color by category; "Seats" is capacity
    shape = "shelter_type",        # shape by category (BDG / SR)
    col = "black",
    fill_alpha = 0.9,
    size = "Seats",
    fill.scale = tm_scale_continuous(
      values = viridis(8, direction = -1)
    ),
    shape.scale = tm_scale_categorical(
      values = c("BDG" = 22, "SR" = 21),  # BDG = square, SR = circle
      labels = c("public (BDG)", "private (SR)")
    ),
    size.scale = tm_scale_continuous(values.scale = 1.3),
    fill.legend = NULL, #tm_legend(title = "Capacity (Seats)"),
    size.legend = NULL,
    shape.legend = tm_legend(title = "Shelter Type")
  ) +
  
  
  # --- Title, legend, and layout ---
  tm_legend(
    title = "Aarhus City: Buildings and Civil Defense Shelters (1952)",
    size = 1.2
  ) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.text.size = 0.8,
    legend.title.size = 1,
    frame = FALSE,
    bg.color = "white"
#   ) 
# 
# tm_layout(
#   frame = TRUE,
#   bg.color = "grey85"
 ) +
  tm_compass(type = "8star", position = c("right", "bottom")) +
  tm_scalebar(position = c("right", "bottom"))
