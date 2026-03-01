################--  Static map with an inset and public shelters in 1952!
library(sf)
library(dplyr)
library(tmap)
library(tidyverse)
library(terra)
library(mapview)

year_filter <- 1952  # or any year you want to filter by



  ### ------------- - First test map -----------------------------------------
  
  
  
# Plot layers
png("test.png", width = 480, height = 520, units = "px")

plot(roads["road_type"], col = road_cols, lwd = c(2,1.5,1))
plot(rail, col = "black", lwd = 1, add = TRUE)
plot(city$geometry, col = NA, border = "darkblue", lwd = 2, add = TRUE)
plot(bld$byg404Koordinat, pch = 21, cex = 0.4,
     col = "grey85", bg = "grey1", add = TRUE)


legend("topright",
       legend = c("City boundary",
                  "Rail",
                  names(road_cols),
                  "Buildings"),
       col = c("darkblue",
               "black",
               road_cols,
               "grey70"),
       lwd = c(2, 1, 2, 1.5, 1, NA),
       pch = c(NA, NA, NA, NA, NA, 21),
       pt.cex = c(NA, NA, NA, NA, NA, 0.8),
       bty = "n")
dev.off()
  




# ----------------
  
library(sf)
library(tmap)
library(dplyr)

# --- tmap v4 setup -----------------------------------------------------------
tmap_mode("plot")   # static map output
# If you want an interactive view instead: tmap_mode("view")

# --- (Optional) filter layers to "1952" -------------------------------------
# Adjust to your real column names (e.g. bld$Year or roads$Year)
# bld_1952   <- bld   %>% filter(Year <= 1952)
# roads_1952 <- roads %>% filter(Year <= 1952)
# rail_1952  <- rail  %>% filter(Year <= 1952)

# --- Ensure everything is in the same CRS (use city's CRS as the reference) --
target_crs <- sf::st_crs(city)

roads_1952 <- st_transform(roads, target_crs) %>% 
  st_intersection(city$geometry)
rail_1952  <- st_transform(rail,  target_crs)%>% 
  st_intersection(st_buffer(city$geometry, 500))
bld_1952   <- st_transform(bld,   target_crs)%>% 
  st_intersection(st_buffer(city$geometry, 500))


plot(rail_1952$geom)

# --- Road styling ------------------------------------------------------------
# Your original: road_cols and widths for 3 classes
# Provide a robust fallback palette if you haven't defined road_cols:
road_levels <- sort(unique(roads_1952$road_type))

road_cols <- setNames(
  c("grey75", "grey85", "grey95")[seq_along(road_levels)],
  road_levels
)
road_lwd <- setNames(
  c(1, 0.6, 0.3)[seq_along(road_levels)],
  road_levels
)


##############################################################
#                     MAPS
##############################################################

# --- MAIN MAP WITH ROADS (Aarhus) COLOR -------------------------------------------------------
m_main <-
  tm_shape(city) +
  tm_borders(
    col = "pink",
    lwd = 3,
    col.legend = tm_legend(title = "City boundary")
  ) +
  tm_shape(roads_1952) +
  tm_lines(col = "road_type",
           lwd = "road_type",
           col.scale  = tm_scale_categorical(values = road_cols),
           lwd.scale  = tm_scale_categorical(values = road_lwd),
           #col.legend = tm_legend(title = ""),  # uncomment to view legend
           col.legend = tm_legend_hide(),
           lwd.legend = tm_legend_hide()) +

  tm_shape(rail_1952) +
  tm_lines(
    col = "darkred",
    lwd = 1.2,
    col.legend = tm_legend(title = "Rail")
  ) +
  
  tm_shape(bld_1952) +
  tm_symbols(
    shape = 21,
    col = "grey30",
    fill = "grey85",
    size = 0.075,
    fill_alpha = 0.8,
    col.legend = tm_legend(title = "Buildings"),
    fill.legend = tm_legend_hide()
  ) +
  
  tm_shape(shelters) +
  tm_symbols(shape = 24,
             col = "red",
             fill = "pink",
             size = 0.35,
             col.legend = tm_legend(title = "Shelters"),
             fill.legend = tm_legend_hide()
  ) +
  
  
  tm_add_legend(type = "lines", labels = "Railway", col = "darkred", lwd = 1.2) +
  tm_add_legend(type = "lines", labels = "City boundary", col = "pink", lwd = 3) +
  tm_add_legend(type = "symbols", labels = "Building",
                col = "grey30", fill = "grey85", shape = 21, size = 0.2) +
  tm_add_legend(type = "symbols", labels = "Shelter",
                col = "red", fill = "pink", shape = 24, size = 0.35) +
  
  tm_title("Aarhus (1952)") +
  tm_layout(
    frame = FALSE,
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.85,
    inner.margins = c(0.02, 0.02, 0.02, 0.02)
  ) +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scalebar(position = c("left", "bottom"),
              breaks = c(0, 0.5, 1.0, 1.5),  # 3 segments of 0.5 km
              text.size = 0.7) 

m_main

#######################################################################

# --- MAIN MAP WITH ROADS (Aarhus) BW -------------------------------------------------------
m_main_bw <-
  tm_shape(city) +
  tm_borders(
    col = "grey",
    lwd = 3,
    col.legend = tm_legend(title = "City boundary")
  ) +
  tm_shape(roads_1952) +
  tm_lines(col = "road_type",
           lwd = "road_type",
           col.scale  = tm_scale_categorical(values = road_cols),
           lwd.scale  = tm_scale_categorical(values = road_lwd),
          # col.legend = tm_legend(title = ""),
           col.legend = tm_legend_hide(),
           lwd.legend = tm_legend_hide()) +
  
  tm_shape(rail_1952) +
  tm_lines(
    col = "black",
    lwd = 2,
    lty="twodash",  #1 = dotdash, 2 = dotted, 3 = twodash, none = longdash
    col.legend = tm_legend(title = "Rail")
  ) +
  
  
  tm_shape(bld_1952) +
  tm_symbols(
    shape = 21,
    col = "grey65",
    fill = "grey85",
    size = 0.075,
    fill_alpha = 0.8,
    col.legend = tm_legend(title = "Building"),
    fill.legend = tm_legend_hide()
  ) +
  tm_shape(shelters) +
  tm_symbols(shape = 24,
             col = "black",
             fill = "white",
             size = 0.35,
             col.legend = tm_legend(title = "Shelter"),
             fill.legend = tm_legend_hide()
  ) +
  
  
  tm_add_legend(type = "lines", labels = "Railway", col = "black", lwd = 2, lty="twodash") +
  tm_add_legend(type = "lines", labels = "City boundary", col = "grey", lwd = 3) +
  tm_add_legend(type = "symbols", labels = "Building",
                col = "grey85", fill = "grey85", shape = 21, size = 0.2) +
  tm_add_legend(type = "symbols", labels = "Shelter",
                col = "black", fill = "white", shape = 24, size = 0.35) +
  
  tm_title("Aarhus (1952)") +
  tm_layout(
    frame = FALSE,
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.85,
    inner.margins = c(0.02, 0.02, 0.02, 0.02)
  ) +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scalebar(position = c("left", "bottom"),
  breaks = c(0, 0.5, 1.0, 1.5),  # 3 segments of 0.5 km
  text.size = 0.7) #+
  # tm_credits(
  #   text = "Author: Adela Sobotkova\nData: shelters and city border digitized by MELICA 2024 on basis of CDC Archive 1952; roads based on OSM 2026, buildings on BBR 2023, historical railways from Fentner 2013",
  #   position = c("left", "bottom"),
  #   size = 0.6
  # )

m_main_bw

# --- INSET MAP (Denmark + highlight Aarhus extent) ---------------------------
# You need a Denmark polygon sf object (call it dk). If you don’t have it:
# dk <- rnaturalearth::ne_countries(country = "Denmark", returnclass = "sf") %>%
#       st_transform(target_crs)

# Inset rectangle = bounding box of the city map, expanded a touch
bbox_city <- st_as_sfc(st_bbox(city))  %>%  st_transform(target_crs)
bbox_city <- aarhus_bbox_m  %>%  st_transform(target_crs)
dk_bbox <- st_transform(dk_bbox, target_crs) %>% 
  st_buffer(5000)

m_inset <-
  tm_shape(st_simplify(dk32,dTolerance = 100), bbox = dk_bbox) +
  tm_polygons(fill = "grey98", border.col = "grey65", lwd = 1.5,
              col.legend = tm_legend_hide()) +
  tm_shape(bbox_city) +
  tm_polygons(border.col = "grey1",
               fill = "grey10",
              lwd = 2) +
  tm_layout(frame = TRUE, legend.show = FALSE, inner.margins = 0)
m_inset


#### --- EXPORT FOR REVIEW  ---

tmap_save(m_main, "Aarhus_col_r0.png", width = 6, height = 8, units = "in", dpi = 600)

tmap_save(m_main_bw, "Aarhus_bw_r3.png", width = 6, height = 9, units = "in", dpi = 600)


############## -----------------  PRINT IT OUT - Figure 
library(grid)


# Export the combined map as a TIFF at 400 DPI
#tiff("../figures/Fig02_bw.tiff", width = 7, height = 10, units = "in", res = 400)

tiff("../figures/Fig02_col.tiff", width = 7, height = 10, units = "in", res = 400)
#png("Fig02_bw.png", width = 7, height = 10, units = "in", res = 600)

# Draw the main map
print(m_main)

# Overlay the inset map
vp_inset <- viewport(width = 0.2, height = 0.2, x = 0.03, y = 0.72, just = c("left", "bottom"))
print(m_inset, vp = vp_inset)

# Close the TIFF device
dev.off()

############## ------------------------------------- 









