library(tidyverse)
library(skimr)
library(sf)
library(maps)
library(ggmap)
library(mapdata)
library(readxl)
library(ggthemes)
library(lubridate)

station_rollouts <- read_excel("Full Network to Date Station Install Dates - 2_1_22.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(
    community = (community_area %>% str_to_title()),
    station = name,
    rollout_year = format(as.Date(install_date, format="%Y/%m/%d"),"%Y", ),
    rollout_month = format(as.Date(install_date, format="%Y/%m/%d"),"%m", )
  ) %>% 
  select(station, community, rollout_month, rollout_year, lat, lon) %>% 
  arrange(community)%>% 
  filter(!is.na(community))

# remove rows where community_area is NA
# community_area is NA because stations are in Evanston, not Chicago
station_rollouts %>% 
  filter(is.na(community))

station_rollouts <- station_rollouts %>% 
  filter(!is.na(community))



inner_join(station_rollouts, communities)

full_join(station_rollouts, communities, by = c("community_area" = "community")) %>% 
  mutate(station = name) %>% 
  select()

station_rollouts$name %>% missing()

communities <- readRDS("data/communities.RDS")
communities[order(communities$community),]


library(mapview)
mapview(communities) + mapview(station_rollouts, xcol = "lon", ycol = "lat")

mapview(communities) %>% 
  ggplot(data = station_rollouts, mapping = aes(lon, lat)) + 
  geom_point()





