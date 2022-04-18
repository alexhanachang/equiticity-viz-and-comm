divvy_stations_density <- read.csv("data/unprocessed data/divvy_data/comm_density_2021.csv")
divvy_demographics <- readRDS("data/divvy_demographics.RDS")
communities <- readRDS("data/communities.RDS")

divvy_demographics_for_density <- divvy_demographics %>% st_drop_geometry()

divvy_stations_density <- divvy_stations_density %>% 
  filter(!is.na(community_area)) %>% 
  mutate(community_area = factor(community_area) %>% str_to_title())

divvy_stations_density[39,2] <- "McKinley Park"

divvy_stations_density <- communities %>% 
  left_join(divvy_stations_density, by = c("community" = "community_area")) %>% 
  select(-X) %>% 
  select(community_id, community, region, avg_in_2_mi_radius) 

divvy_stations_density <- divvy_stations_density %>% 
  left_join(divvy_demographics_for_density) %>% 
  select(community_id, community, region, avg_in_2_mi_radius, population)

write_rds(divvy_stations_density, "data/divvy_stations_density.RDS")
write_rds(divvy_stations_density, "app/data/divvy_stations_density.RDS")
