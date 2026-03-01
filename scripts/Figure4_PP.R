### 1952 ABM results


# Figures 4 & 5 - within 5 min, nearest 3 shelters
# city with districts (postal codes) and shelters showing unused ones
# close up on Ringgade with shelter sizes and colorshading showing under/over utilisation


### ---------------- load ABM data and districts 
day <- read_rds("../data/abm/shelter_arrivals_ranked_day_baseline_nearest_3.RDS")
night <- read_rds("../data/abm/shelter_arrivals_ranked_night_baseline_nearest_3.RDS")
head(day)

library(purrr)

summary_day <- day %>%
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


shelters5min <- shelters %>% 
  left_join(summary_day, by = c("shelterID" = "shelter_id")) %>% 
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

# test day sheltering results
mapview(shelters5min, zcol = "status_5min", cex = "pct_used_5min") # are all our agents unique (only shelter  once?)


### ---------- MAP library(tmap)
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
      
# Divergent map
load_pct <- tm_shape(st_transform(city, 4326)) +
  tm_borders(
    col = "grey",
    lwd = 3,
    col.legend = tm_legend(title = "City boundary")
  ) +


tm_shape(shelters_5m_cap) +
  tm_symbols(
    fill  = "pct_dev_5min",         #pct_used_5min is another option for percentage used in 5 mins
    shape = 21,
    
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
  tm_title("Shelter (nearest-3) load at 5 minutes") +
  tm_scalebar(position = c("left", "bottom"),
              breaks = c(0, 0.5, 1.0, 1.5),  # 3 segments of 0.5 km
              text.size = 0.7) 

load_pct

tmap_save(load_pct, "load_5m_pct.png", width = 8, height = 8, units = "in", dpi = 600)

# Working ascending scale for overload only

overload_pct <- tm_shape(st_transform(city, 4326)) +
  tm_borders(
    col = "grey",
    lwd = 3,
    col.legend = tm_legend(title = "City boundary")
  ) +
  tm_shape(shelters_5m_cap) +
  tm_symbols(
    fill  = "pct_used_5min",         #pct_used_5min is another option for percentage used in 5 mins
    shape = 21,
    
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
  tm_title("Shelter (nearest-3) load at 5 minutes") +
  tm_scalebar(position = c("left", "bottom"),
              breaks = c(0, 0.5, 1.0, 1.5),  # 3 segments of 0.5 km
              text.size = 0.7) 

tmap_save(overload_pct, "overload_5m_pct.png", width = 8, height = 8, units = "in", dpi = 600)

### not used below yet

m_cap_5 <- tm_shape(shelters_5m_cap) +
  tm_symbols(
    fill  = "diff_5min",                  # arrivals_5min - Seats
    size = "arrivals_5min",              # optional: or use "abs_diff_5min" (see below)
    shape = 21,
    fill.scale  = tm_scale_continuous(
      midpoint = 0,
      values = c("steelblue4", "white", "firebrick3")   # under -> at -> over
    ),
    col.legend  = tm_legend(title = "5-min capacity\n(+ over, − under)"),
    size.scale  = tm_scale_continuous(values = c(0.03, 0.12)),
    size.legend = tm_legend(title = "Arrivals ≤ 5 min")
  ) +
  tm_layout(frame = FALSE) +
  tm_title("Shelter load at 5 minutes") +
  tm_scalebar()

m_cap_5