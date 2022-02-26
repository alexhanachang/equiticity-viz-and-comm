library(sf)
library(tidyverse)

communities <- read_sf('data/violence_enforcement/communities/geo_export_45d4da8d-3fc8-4ccb-9b0c-8f546d526c9a.shp')

regions <- tibble(
  community_id = c(1:77),
  region = c(
    "Far North Side", "Far North Side", "Far North Side", "Far North Side", 
    "North Side", "North Side", "North Side", 
    "Central", 
    "Far North Side", "Far North Side", "Far North Side", "Far North Side", "Far North Side", "Far North Side", 
    "Northwest Side", "Northwest Side", "Northwest Side", "Northwest Side", "Northwest Side", "Northwest Side",
    "North Side", "North Side", 
    "West Side", "West Side", "West Side", "West Side", "West Side", "West Side", "West Side", "West Side", "West Side", 
    "Central", "Central", 
    "South Side", "South Side", "South Side", "South Side", "South Side", "South Side", "South Side", "South Side", "South Side", "South Side",
    "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", "Far Southeast Side", 
    "Southwest Side", "Southwest Side", "Southwest Side", "Southwest Side", 
    "South Side",
    "Southwest Side", "Southwest Side", "Southwest Side", "Southwest Side", "Southwest Side", "Southwest Side", "Southwest Side", "Southwest Side", 
    "South Side",
    "Far Southwest Side", "Far Southwest Side", "Far Southwest Side", "Far Southwest Side", "Far Southwest Side", "Far Southwest Side",
    "Far North Side", "Far North Side"
  ))

communities <- communities %>% 
  mutate(
    community = (communities$community %>% str_to_title()),
    community_id = area_numbe,
    community_id = as.numeric(community_id)
  ) %>% 
  arrange(community_id) %>% 
  left_join(regions) %>% 
  select(community_id, community, region, geometry) 

# rename Mckinley Park to McKinley Park
filter(communities, community == "Mckinley Park")
communities[59, 2] <- "McKinley Park"

# save
saveRDS(communities, "data/communities.RDS")
