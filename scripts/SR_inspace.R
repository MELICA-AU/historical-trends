### ----  SPATIAL OVERVIEW Of PUBLIC AND PRIVATE SHELTERS  IN AARHUS
#
# In this script I take the SR, NDG and KOB data for Aarhus, and 
# map them them in space and time to capture the diachronic development
# of CD network in the city since 1950s

## Maps in Tmap (for overviews see HistoricalSR end)
### SR data from Ulrik (SR) with sikringsrum, processed in 07_HistoricalSR


# library 
library(tidyverse)
library(sf)
library(mapview)
library(tmap)


### ------------- LOAD SR PRIVATE SHELTERS LOCAL

SR <- st_read("../shelter-data/output_data/SR.geojson")
names(SR)

mapview(SR)

SR_89   <- SR %>%
  mutate(decade = case_when(
    decade == '1930s' ~ '1180-1939',
    TRUE ~ decade  # Keep the original value for other cases
  )) %>% 
  dplyr::filter(decade < "1990s" & decade > "1180-1939") 

table(SR_89$decade)
table(SR_89$year)

SR %>% 
  #SR_89 %>% 
  group_by(decade) %>% 
  summarize(sum = n())



### -------------  LOAD COMBINED PUBLIC AND PRIVATE SHELTER (KOB) data from 2024

KOB <- readRDS("../shelter-data/output_data/KOB.rds")
mapview(KOB, cex = "Capacity", zcol = "Year")


# ### ------------- LOAD Public shelter data
# 
# # Load verified and unverified data from 2023
# verified <- st_read("output_data/TF_verified23.geojson") # very liberal  (some duplicates but spatially in same spot)
# unverified <- st_read("output_data/TF_unverified.geojson") # very conservative 
# unverified <- st_read("output_data/2024_needlgv.geojson")
# 
# verified %>% 
#   select(FeatureID, Source) %>% 
#   merge(unverified$first_number) %>% 
#   mapview(Source)
# 
# unverified <- unverified %>% 
#   dplyr::select(geometry) %>% 
#   mutate(Verified = "No")
# verified <- verified %>%
#   dplyr::select(geometry) %>% 
#   distinct() %>% 
#   mutate(Verified = "Yes") 
# 
# # Step 2: Merge the two sf objects into a single sf object
# sh_merged <- rbind(verified, unverified)
# sh_merged <- sh_merged %>% 
#   st_transform(25832)
# sh_merged$Verified <- as.factor(sh_merged$Verified)
# 
# saveRDS(sh_merged, "output_data/sh_merged.rds") # only points and 2024 verification status of public shelters
# # View the result
# sh_merged <- readRDS("output_data/sh_merged.rds") # only points and 2024 verification status of public shelters
# 
# mapview(sh_merged, zcol = "Verified")

### ------------- LOAD BDG LONG data from 2024

# All shelters sheet with long format where temporal changes are represented

BDG <- readRDS("../shelter-data/output_data/BDG_long.rds")
glimpse(BDG)

### -------------  LOAD BDG WIDE data from 2024

# All shelters sheet with long format where temporal changes are represented
BDGw <- readRDS("../shelter-data/output_data/BDG_wide2024.rds") %>% 
  rename(verified = FAIMS_verified)
glimpse(BDGw)


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


mapview(aa_1988) + mapview(aa_1952, col.regions = "orange")+ mapview(aa_1956, col.regions = "pink")

############## --------------- Create a full map interactive

conflicts(detail = TRUE) |>
  purrr::keep(~"tm_shape" %in% .x)


tmap_mode(mode = "plot")

tm_shape(shp = SR_89) +
  #tm_basemap("CartoDB.Positron") + # CartoDB.Positron as the basemap
  tm_dots(
    fill = "decade",
    size = "places",
    fill_alpha = 0.8,
    fill.scale = tm_scale(values = "viridis", label = "Decade"),
    fill.legend = tm_legend(position = c("right", "bottom"))) +
  tm_shape(aarhus_bbox_sm) +
  tm_borders(lwd = 2, col = "red") +
  tm_shape(aa_1988) +
  tm_borders(lwd = 2, col = "orange") +
  tm_title("Private shelter construction in Aarhus") +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_layout(frame = FALSE)



################------------------------------ Static map with an inset and public shelters!

tmap_mode(mode = "plot")

main_map <-
  tm_shape(dk, bbox = aarhus_bbox_m) +
  tm_polygons(fill = "white") +
  
  tm_shape(SR_89) +
  tm_dots(
    fill = "decade",
    size = "places",
    fill_alpha = 0.8,
    fill.scale = tm_scale(values = "viridis", label = "Decade"),
    #size.scale = tm_scale_continuous(label = "Places", values.scale = 1.5),
    fill.legend = tm_legend(orientation = "portrait")
  ) +
  
  tm_shape(BDGw) +
  tm_squares(
    size = 0.1,
    fill = "verified",
     fill.scale = tm_scale(values = c("white", "grey9"))) +
  # city borders over time
  tm_shape(aa_1952) +
  tm_borders(lwd = 1, col = "darkgrey", lty = "dotted") +
  tm_shape(aa_1988) +
  tm_borders(lwd = 2, col = "grey") +
  
  tm_title("Shelter construction in Aarhus") +
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
tiff("figures/SK_BTG_decade_map.tiff", width = 7, height = 10, units = "in", res = 400)

# Draw the main map
print(main_map)

# Overlay the inset map
vp_inset <- viewport(width = 0.3, height = 0.3, x = 0.1, y = 0.01, just = c("left", "bottom"))
print(inset_map, vp = vp_inset)

# Close the TIFF device
dev.off()

############## -------------------------------------  TMAP FACETTED PRIVATE SHELTERS OVER DECADES

library(tmap)

tmap_mode("plot")

facetted_map <-
  tm_shape(st_as_sf(dk), bbox = aarhus_bbox_m) +
  tm_borders(lwd = 2, col = "grey") +
  tm_polygons(fill = "white") +
  tm_shape(aa_1952) +
  tm_borders(lwd = 1, col = "darkgrey", lty = "dotted") +
  tm_shape(SR_89) +
  tm_dots(
    fill = "decade",
    size = "places",
    fill.scale = tm_scale(values = "viridis"),
   # size.scale = tm_scale_continuous(label = "Capacity", values.scale = 1.5),
    fill_alpha = 0.8,
    fill.legend = tm_legend_hide()
  ) +
  
  tm_facets(
    by = "decade",
    ncol = 3,
    nmax = 6,
    free.coords = FALSE
  ) +
  
  tm_layout(
    bg.color = "grey80",
    panel.label.size = 1.5,
    outer.margins = 0.02,
    inner.margins = 0.05,
    legend.outside = TRUE,
    legend.outside.position = "bottom"
  ) +
  
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  
  tm_scalebar(
    breaks = c(0, 5),
    text.size = 0.8,
    position = c("left", "bottom")
  )

# View the facetted map
facetted_map



# Save the facetted map as a high-resolution PNG
#tmap_save(facetted_map, "facetted_map.png", width = 10, height = 8, dpi = 300)

# Or save it as a PDF
tmap_save(facetted_map, "figures/SK_facetted_map.tiff", width = 8, height = 8, dpi = 400)

