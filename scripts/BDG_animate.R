### ----  ANIMATE BDG MOVEMENT IN SPACE IN AARHUS
#
# In this script I take the BDG data for Aarhus, and animate their diachronic development
# especially the movement of the relocatable ones in 1960-1990s
# Challenges:
## 1. Preserve the stationary shelters in the background
## 2. Avoid jerky motion of the animation

# library
library(gganimate)
library(tidyverse)

### ------------- LOAD BDG DATA LOCAL

BDG <- readRDS("../shelter-data/output_data/BDG_long.rds")

# Lengthen BDG dataset
expanded_BDG <- BDG %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2024) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 2))) %>%
  unnest(year) %>%
  select(-end_year)

glimpse(expanded_BDG)


##### -------------  ANIMATE 

# Animation one (too fast and choppy)
anim_bdg <- ggplot(data = expanded_BDG )+
  geom_sf(aes(color = Type))+
  theme_bw() +
  transition_time(year)+
  labs(subtitle = "Year {round(frame_time,0)}")
anim_bdg 


#### -------------  Attempt no.1

# Static map: Preserve the stationary shelters in the background 
border <- st_read("data/Topo/BordersShapes/Poly1952.shp")

p <- ggplot() +
  geom_sf(data = border, aes(alpha = 0.5),show.legend = FALSE)+
  geom_sf(data = BDG %>%filter (Location_startdate == 1944) %>% st_geometry(), aes(col = "grey"),show.legend = FALSE)+
  geom_sf(data = BDG, aes(group = BDnr, color = Type), size = 2, show.legend = FALSE) +
  labs(title = 'Year: {round(frame_time,0)}') +
  theme_minimal()
p
# Animate the plot
anim <- p +
  transition_time(Location_startdate) + 
  ease_aes('linear')  # Ensures smooth transition

anim

# Render the animation (takes too long)
animate(anim, duration = 20, fps = 10, width = 500, height = 700)


#### -------------  Attempt no.2 

# https://stackoverflow.com/questions/68450668/how-can-i-animate-points-on-a-spatial-map-with-gganimate-sf-and-ggplot2
#https://stackoverflow.com/questions/37397303/change-label-of-gganimate-frame-title

## Static map: Preserve the stationary shelters in the background 
border <- st_read("data/Topo/BordersShapes/Poly1952.shp")

B <- ggplot() +
  geom_sf(data = border, aes(alpha = 0.5),show.legend = FALSE)+
  #geom_sf(data = BDG$geometry, aes(col = "grey", size = 0.5), show.legend = FALSE)+
  geom_sf(data = expanded_BDG, aes(col = "grey", size = 0.5),
          show.legend = FALSE) +
  labs(title = 'Year: {closest_state}') +
  theme_minimal()
B
##Create animation with points showing up one by one
plot_anim <- B +
 
  transition_states(states = year, state_length = 1, wrap = FALSE) +
  enter_recolor(fill = "#f0f5f9") +
  shadow_mark(past = TRUE, alpha = 1, fill = "#3a6589")

##Render animation
animate(plot_anim, end_pause = 60,
        height = 500, width = 500) # a higher res img would not upload here :(

# save animated map
anim_save("animated_map4.gif")



################ SPLIT THE DATA

static_shelters <- BDG %>% 
  group_by(BDnr) %>% 
  filter(n() == 1) %>%  # Assumes there's only one record for static shelters
  ungroup()

static_BDG <-static_shelters %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2010) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 1))) %>%
  unnest(year) %>%
  select(-end_year)


moving_shelters <- BDG %>% 
  group_by(BDnr) %>% 
  filter(n() > 1) %>%  # More than one record indicates movement
  ungroup()

unique(moving_shelters$Location_startdate)

moving_BDG <- moving_shelters %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2024) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 1))) %>%
  unnest(year) %>%
  select(-end_year)


# Create the base plot with static shelters
p <- ggplot() +
  geom_sf(data = border, aes(alpha = 0.5),show.legend = FALSE)+
  geom_sf(data = static_BDG$geometry, color = "darkgrey", size = 0.5) + 
  labs(title = 'Year: {round(frame_time,0)}') +
  theme_minimal()

# Add moving shelters with animation
p2 <- p + 
  geom_sf(data = moving_BDG, aes(color = Type), size = 1, show.legend = FALSE) +
  transition_time(year) +
  ease_aes('cubic-in-out') +  # Change easing function for smoother transition
  shadow_mark(past = TRUE, future = FALSE, size = 1, alpha = 0.3)  # Show past positions as shadow
p2

# Save the animation if needed
anim_save("animated_map5.gif", animation = p2)

# Render the animation
anim <- animate(p, duration = 20, fps = 10, width = 500, height = 500)
