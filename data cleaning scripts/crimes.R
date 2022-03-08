# crash data
crash_data <- read_csv("data/unprocessed data/violence_enforcement/2021_crash_communities.csv") %>% 
  mutate(
    community = str_to_title(community),
    community = ifelse(community == "Ohare", "O'Hare", community),
    community = ifelse(community == "Mckinley Park", "McKinley Park", community)
  ) %>% 
  rename("crash_count" = "n") %>% 
  rename("community" = "community") %>% 
  filter(!is.na(community))

communities %>% 
  left_join(crash_data) %>% 
  select(community_id, community, region, crash_count, geometry)


# crimes data - relevant
relevant_crimes <- read_csv("data/unprocessed data/violence_enforcement/crimes_2013-2021/relevant_crimes.csv")

relevant_crimes_community <- relevant_crimes %>% 
  mutate(community = NA) %>% 
  group_by(community_area, community, year) %>% 
  select(-location_description) %>% 
  count() %>% 
  filter(!is.na(community_area))

relevant_crimes_community <- communities %>% 
  left_join(relevant_crimes_community, by = c("community_id" = "community_area")) %>% 
  mutate(community = community.x) %>% 
  mutate(num_crimes = n)

relevant_crimes_community <- st_drop_geometry(relevant_crimes_community) %>% 
  select(community_id, community, region, year, num_crimes)


# summing total crime statistics yearly
all_chicago <- relevant_crimes %>% 
  mutate(community = "All - Chicago") %>% 
  group_by(year, community) %>% 
  count() %>% 
  mutate(num_crimes = n)

all_chicago$community_id <- NA
all_chicago$region <- NA

all_chicago <- all_chicago %>% 
  mutate(community_id = community_id %>% as.numeric(), region = region %>% as.character() %>% factor()) %>% 
  select(community_id, community, region, year, num_crimes)

relevant_crimes_community <- rbind(all_chicago, relevant_crimes_community)

# creating labels for UI
community_labs <- relevant_crimes$community




# counting crimes
crime_statistics <- relevant_crimes %>% 
  group_by(community_area) %>% 
  summarise(num_crimes = n()) %>% 
  filter(!is.na(community_area)) 

# finding crime rate and other statistics
crime_statistics <- census %>% 
  left_join(crime_statistics, by = c("community_id" = "community_area")) %>% 
  select(community_id, community, region, num_crimes, num_of_divvy_stations, num_docks_in_service, pop, area_sq_mi, geometry)

crime_statistics <- crime_statistics %>% 
  mutate(crime_rate = (num_crimes / pop) * 100000,
         divvy_to_pop = (num_of_divvy_stations / pop) * 100000,
         divvy_to_area = num_of_divvy_stations / area_sq_mi) %>% 
  select(community_id, community, region,
         num_crimes, num_of_divvy_stations, num_docks_in_service, pop, area_sq_mi, 
         crime_rate, divvy_to_pop, divvy_to_area, 
         geometry)

# finding the center of each community for geom_point
crime_statistics <- crime_statistics %>%
  mutate(geometry_center = st_centroid(geometry))

crime_statistics <- crime_statistics %>%
  mutate(lat_lon = st_coordinates(geometry_center),
         lat = lat_lon[,"X"],
         lon = lat_lon[,"Y"]) %>% 
  select(-c(geometry_center, lat_lon))

#--------------------------------

# police sentiment
police_sentiment_data <- read_csv("data/unprocessed data/violence_enforcement/sentiment_eda.csv") %>% 
  mutate(community_id = 1:77, community = Community) %>% 
  arrange(community_id) %>% 
  select(-Community) %>% 
  select(community_id, community, everything())

write_rds(relevant_crimes_community, "data/relevant_crimes_community.RDS")
write_rds(relevant_crimes_community, "app/data/relevant_crimes_community.RDS")

write_rds(crime_statistics, "data/crime_statistics.RDS")
write_rds(crime_statistics, "app/data/crime_statistics.RDS")

write_rds(police_sentiment_data, "data/police_sentiment_data.RDS")
write_rds(police_sentiment_data, "app/data/police_sentiment_data.RDS")

