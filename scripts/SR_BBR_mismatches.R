### -------------  SR Check 
library(googlesheets4)

gs4_deauth()
SR <- read_sheet("https://docs.google.com/spreadsheets/d/1W8vrCWUhpqM-ELa0NspSpSdwq4hsZTIAK1mszBPmP8g/edit?gid=1148112611#gid=1148112611")

SR %>% 
  group_by(capacity_agrees_with_BBR) %>% 
  tally() %>% 
  mutate(pct = round(n/1663 * 100, 1))
