library(sf)
library(tidyverse)

communities <- read_sf('data/violence_enforcement/communities/geo_export_45d4da8d-3fc8-4ccb-9b0c-8f546d526c9a.shp')

communities <- communities %>% 
  mutate(community = (communities$community %>% str_to_title())) %>% 
  mutate(community_id = area_numbe) %>% 
  mutate(community_id = as.numeric(community_id)) %>% 
  select(community_id, community, geometry) %>% 
  arrange(community_id)

saveRDS(communities, "data/communities.RDS")