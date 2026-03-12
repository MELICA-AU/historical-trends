### 1952 ABM results
library(sf)
library(tidyverse)

# Figures 4 & 5 - within 5 min, nearest 3 shelters
# city with districts (postal codes) and shelters showing unused ones
# close up on Ringgade with shelter sizes and colorshading showing under/over utilisation

source("01_LoadDataPP.R")

### ---------------- load ABM data and districts 
night <- read_rds("../data/abm/shelter_arrivals_ranked_night_baseline_nearest_3.RDS")
districts <- read_sf("../data/postnummerinddeling.geojson") %>% 
  st_intersection(st_geometry(st_transform(city, 4326)))

plot(districts$geometry)
centroids <- st_centroid(districts)

library(purrr)

summary_night <- night %>%
  imap_dfr(~ .x %>%
             summarise(
               arrivals_5min  = sum(arrival_time <= 300, na.rm = TRUE),
               arrivals_over5min  = sum(arrival_time > 300, na.rm = TRUE),
               arrivals_10min = sum(arrival_time <= 600, na.rm = TRUE)
             ) %>%
             mutate(shelter_id = .y),
           .id = NULL) %>%
  relocate(shelter_id) %>% 
  as_tibble()


### ------------ night TIME SHELTERING RESULTS -------------------

shelters5min <- shelters %>% 
  left_join(summary_night, by = c("shelterID" = "shelter_id")) %>% 
  mutate( # core metrics
    diff_5min = arrivals_5min - Seats,                 # + = over, - = under
    pct_used_5min = 100 * arrivals_5min / Seats,       # % of capacity used
    
    # guard rails
    pct_used_5min = if_else(is.finite(pct_used_5min), pct_used_5min, NA_real_),
    
    status_5min = case_when(
      is.na(Seats) ~ "missing capacity",
      Seats <= 0 ~ "zero capacity",
      diff_5min > 0 ~ "over capacity",
      diff_5min == 0 ~ "at capacity",
      diff_5min < 0 ~ "under capacity"
    )
  )

which(is.na(shelters5min$arrivals_10min))

shelters_5m_cap <- shelters5min %>%
  filter(!is.na(arrivals_5min)) %>% 
  mutate(
    diff_class = cut(
      diff_5min,
      breaks = c(-Inf, -1, 0, 1, Inf),
      labels = c("under", "at", "at", "over"),
      right = TRUE
    ),
    over_by = pmax(diff_5min, 0),          # how many over
    under_by = pmax(-diff_5min, 0)         # how many under
  )

# test view of night sheltering results
mapview(shelters5min, zcol = "status_5min", cex = "pct_used_5min") # are all our agents unique (only shelter  once?)


### ---------- MAPPING nightTIME RESULTS ---------------------

shelters_5m_cap[,6:11]

# Map percent over
shelters_5m_cap <- shelters_5m_cap %>%
  mutate(pct_used_5min_cap = pmin(pct_used_5min, 300),
         pct_dev_5min = pct_used_5min_cap - 100,
         pct_dev_5min = pmax(pmin(pct_dev_5min, 200), -100),  # cap: -100% .. +200%
         # 3 size classes based on capacity (Seats) so small shelters don't vanish
         seats_class = cut(
           Seats,
           breaks = c(-Inf, 50, 200, Inf),
           labels = c("Small", "Medium", "Large"),
           include.lowest = TRUE,
           right = TRUE
         )) %>%   
  # subtracting 100% so as to get at underused shelters
  mutate(
    
  )

#### ------------- Divergent map
library(tmap)

load_pct <- tm_shape(st_transform(city, 4326)) +
  tm_borders(
    col = "grey",
    lwd = 3,
    col.legend = tm_legend(title = "City boundary")
  ) +
  
  tm_shape(districts) +
  tm_borders(
    col = "grey",
    lwd = 1,
    lty = "dashed"
  ) +
  tm_labels("navn",
            size = 1,
            col = "grey"
  ) +
  
  tm_shape(shelters_5m_cap) +
  tm_symbols(
    fill  = "pct_dev_5min",         #pct_used_5min is another option for percentage used in 5 mins
    shape = 24,
    
    size = "seats_class", # size driven by capacity, color by stress
    size.scale = tm_scale_categorical(values = c(Small = 0.5, Medium = 0.9, Large = 1.2)),
    size.legend = tm_legend(title = "Shelter capacity"),
    
    # divergent scale
    fill.scale = tm_scale_continuous(
      midpoint = 0,
      limits = c(-100, 200),
      values = c("forestgreen", "white", "firebrick3")
    ),
    fill.legend = tm_legend(
      title = "",
      labels = c("−100% (empty)", "0 (at capacity)", "+200% (300% used)")
    )
  ) +
  
  tm_layout(frame = FALSE, 
            legend.outside = TRUE,
            legend.outside.position = "right",
            # legend.position = c("right", "middle"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.85,
            legend.stack = "vertical",
            legend.spacing = 1.5        # increase space between legends
  ) +
  tm_title("Shelter (nearest-3) load at 5 minutes: nighttime scenario") +
  tm_scalebar(position = c("left", "bottom"),
              breaks = c(0, 0.5, 1.0, 1.5),  # 3 segments of 0.5 km
              text.size = 0.7) 

load_pct

tmap_save(load_pct, "../figures/night_5m_tri.png", width = 8, height = 8, units = "in", dpi = 600)

### -------------- Sequential scale 

overload_pct <- tm_shape(st_transform(city, 4326)) +
  tm_borders(
    col = "grey",
    lwd = 3,
    col.legend = tm_legend(title = "City boundary")
  ) +
  tm_shape(districts) +
  tm_borders(
    col = "grey",
    lwd = 1,
    lty = "dashed"
  ) +
  tm_labels("navn",
            size = 1,
            col = "grey"
  ) +
  tm_shape(shelters_5m_cap) +
  tm_symbols(
    fill  = "pct_used_5min",         #pct_used_5min is another option for percentage used in 5 mins
    shape = 24,
    
    size = "seats_class", # size driven by capacity, color by stress
    size.scale = tm_scale_categorical(values = c(Small = 0.5, Medium = 0.9, Large = 1.2)),
    size.legend = tm_legend(title = "Shelter capacity"),
    
    fill.scale = tm_scale_continuous(
      limits = c(0, 200),
      values = c("white", "gold", "firebrick3")  # low -> mid -> high
    ),
    fill.legend = tm_legend(title = "% capacity used\n(capped at 200%)")
  ) +
  
  tm_layout(frame = FALSE) +
  tm_title("Shelter (nearest-3) load at 5 minutes: nighttime scenario") +
  tm_scalebar(position = c("left", "bottom"),
              breaks = c(0, 0.5, 1.0, 1.5),  # 3 segments of 0.5 km
              text.size = 0.7) 

overload_pct

tmap_save(overload_pct, "../figures/night_5m_pct_tri.png", width = 8, height = 8, units = "in", dpi = 600)

#######################################################

#########     ---------- FIGURE 05 -- ZOOM in figure

# Zoom to downtown
venue <- sf::st_sfc(sf::st_point(c(10.1934814423502, 56.1587545885577)), crs = 4326)
area <- venue %>% 
  st_buffer(1600)

# Preparation of data
shelters10min_n <- shelters %>% 
  left_join(summary_night, by = c("shelterID" = "shelter_id")) %>% 
  mutate( # core metrics
    diff_10min = arrivals_10min - Seats,                 # + = over, - = under
    pct_used_10min = 100 * arrivals_10min / Seats,       # % of capacity used
    
    # guard rails
    pct_used_10min = if_else(is.finite(pct_used_10min), 
                             pct_used_10min, NA_real_),
    
    status_10min = case_when(
      is.na(Seats) ~ "missing capacity",
      Seats <= 0 ~ "zero capacity",
      diff_10min > 0 ~ "over capacity",
      diff_10min == 0 ~ "at capacity",
      diff_10min < 0 ~ "under capacity"
    ),
    seats_class = cut(
      Seats,
      breaks = c(-Inf, 50, 200, Inf),
      labels = c("Small (<50)", "Medium (51-200)", "Large (201-550)"),
      include.lowest = TRUE,
      right = TRUE
    ))

# Map percent over
shelters_10m_capn <- shelters10min_n %>%  # 4 size classes based on overload 
  st_filter(st_buffer(area,500), .predicate = st_intersects)  %>% 
  mutate(overload_class = cut(
    pct_used_10min,
    breaks = c(-Inf, 100, 250, 500, Inf),
    labels = c("≤100%", "101–250%", "251–500%", ">500%"), 
    include.lowest = TRUE,
    right = TRUE ),
    overload_class = factor(
      overload_class,
      levels = c("≤100%", "101–250%", "251–500%", ">500%"),
      ordered = TRUE)) %>% 
  filter(!is.na(overload_class))


### -------------- Sequential scale BLUE and YR; size true


overload_pct_night <- tm_shape(districts, bbox = area) +
  tm_borders(
    col = "grey",
    lwd = 1,
    lty = "dashed"
  ) +
  tm_labels("navn",
            size = 1,
            col = "grey"
  ) +
  tm_shape(shelters_10m_capn) +
  tm_symbols(
    shape = 24,
    
    #size = 2,
    size = "seats_class", # size driven by capacity, color by stress
    size.scale = tm_scale_categorical(values = c(`Small (<50)` = 0.7,
                                                 `Medium (51-200)` = 1.3, 
                                                 `Large (201-550)` = 1.6)),
    size.legend = tm_legend(title = "Shelter capacity"),
    
    fill = "overload_class",
    
    fill.scale = tm_scale_categorical(  #"≤100", "101–250%", "251–500%", ">500%"
      # for yellow, uncomment the lines below
      #   values = c(
      #       "≤100%" = "#ffffcc",
      #       "101–250%" = "#fed976",
      #       "251–500%" = "#fd8d3c",
      #       ">500%" = "#bd0026" )
      # ),
      # for blue, activate the lines below
      values = c(
        "≤100%" = "#f7fbff",
        "101–250%" = "#c6dbef",
        "251–500%" = "#6baed6",
        ">500%" = "#08306b"
      )
    ),

    fill.legend = tm_legend(title = "% capacity used"),
    col = "grey20"
  ) +
  
  tm_title("Shelter (nearest-3) load at 10 minutes: nighttime scenario")  



overload_pct_night

tmap_save(overload_pct_night, "../figures/Fig5_nightload10m_sizetrueBL.png", width = 10, height = 8, units = "in", dpi = 600)
#yellow
tmap_save(overload_pct_night, "../figures/Fig5_nightload10m_sizetrueYR.png", width = 10, height = 8, units = "in", dpi = 600)



### -------------- Sequential scale BLUE and YR; size by overload



overload_size_night <- tm_shape(districts, bbox = area) +
  tm_borders(
    col = "grey",
    lwd = 1,
    lty = "dashed"
  ) +
  tm_labels("navn",
            size = 1,
            col = "grey"
  ) +
  tm_shape(shelters_10m_cap) +
  tm_symbols(
    shape = 24,
    size = "overload_class", # size driven by capacity, color by stress
    size.scale = tm_scale_categorical(values = c(
      "≤100%" = 0.6,
      "101–250%" = 0.9,
      "251–500%" = 1.2,
      ">500%" = 1.5
    )),
    size.legend = tm_legend(title = "Shelter overload"),
    
    fill = "overload_class",
    
    fill.scale = tm_scale_categorical(  #"≤100", "101–250%", "251–500%", ">500%"
      # for yellow, uncomment the lines below
    #   values = c(
    #     "≤100%" = "#ffffcc",
    #     "101–250%" = "#fed976",
    #     "251–500%" = "#fd8d3c",
    #     ">500%" = "#bd0026" )
    # ),
    # for blue, activate the lines below
      values = c(
        "≤100%" = "#f7fbff",
        "101–250%" = "#c6dbef",
        "251–500%" = "#6baed6",
        ">500%" = "#08306b"
      )
    ),
    
    fill.legend = tm_legend(title = "% capacity used"),
    col = "grey20"
  ) +
  
  tm_title("Shelter (nearest-3) load at 10 minutes: nighttime scenario")  


overload_size_night

tmap_save(overload_size_night, "../figures/Fig5_nightload10m_BL.png", width = 10, height = 8, units = "in", dpi = 600)
#yellow
tmap_save(overload_size_night, "../figures/Fig5_nightload10m_YR.png", width = 10, height = 8, units = "in", dpi = 600)
