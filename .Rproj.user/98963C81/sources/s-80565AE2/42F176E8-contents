library(leaflet)
library(leafpop)
library(mapview)
library(sf)
library(skimr)
library(spData)
library(tidyverse)

communities <- readRDS('data/communities.RDS')
crimes <- read_csv('data/violence_enforcement/crimes_2021.csv')

crimes_type <- crimes %>% 
  mutate(community_id = as.numeric(community_area)) %>% 
  group_by(community_id, primary_type) %>% 
  summarise(num_crimes = n()) %>% 
  filter(num_crimes == max(num_crimes))

crimes_type_map <- communities %>% 
  right_join(crimes_type, by = c('community_id' = 'community_id')) %>% 
  select(community_id, community, primary_type, num_crimes, geometry)

mapview(
  crimes_type_map, 
  zcol = "num_crimes", 
  popup = leafpop::popupTable(crimes_type_map, zcol = c("community", "num_crimes")))



