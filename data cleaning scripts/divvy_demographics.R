selected_data_for_viz_com <- read_rds("data/unprocessed data/census/selected_data_for_viz_com.rds")

divvy_demographics <- left_join(communities, selected_data_for_viz_com, by = c("community_id" = "community_id")) %>% 
  mutate(
    community = community.x,
    geometry = geometry.x, 
    msh_index_score = msh_index_score %>% as.numeric()
  ) %>% 
  dplyr:: select(community_id, community, region, everything())

st_geometry(divvy_demographics) <- "geometry"

divvy_demographics <- divvy_demographics %>% 
  dplyr::select(-c(community.x, geometry.x, community.y, geometry.y)) 

write_rds(divvy_demographics, "data/divvy_demographics.RDS")
write_rds(divvy_demographics, "app/data/divvy_demographics.RDS")

