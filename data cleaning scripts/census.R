library(sf)
library(tidyverse)

census <- read_excel("data/unprocessed data/census/neighborhood_info.xlsx")

census$community_id <- 1:nrow(census)

census <- communities %>% 
  left_join(census) %>% 
  select(-neighborhood) %>% 
  select(community_id, community, region, pop, num_of_divvy_stations, num_docks_in_service, area_sq_mi, geometry)

# adjusting census data so it can be joined
census <- census %>% 
  mutate(neighborhood = tolower(neighborhood)) %>% 
  rename(community = neighborhood)

# save
saveRDS(census, "data/census.RDS")
saveRDS(census, "app/data/census.RDS")

