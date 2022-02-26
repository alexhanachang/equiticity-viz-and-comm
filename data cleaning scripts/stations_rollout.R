library(broom)
library(bslib)
library(dplyr)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(leafpop)
library(lubridate)
library(mapdata)
library(maps)
library(mapview)
library(readxl)
library(rgdal)
library(sf)
library(shiny)
library(skimr)
library(spData)
library(thematic)
library(tidyverse)

stations_rollout <- read_excel("drafting visualizations/vis_1/Full Network to Date Station Install Dates - 2_1_22.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(
    community = (community_area %>% str_to_title()),
    station = name,
    rollout_year = format(as.Date(install_date, format="%Y/%m/%d"),"%Y", ),
    rollout_month = format(as.Date(install_date, format="%Y/%m/%d"),"%m", )
  ) %>% 
  select(station, community, rollout_month, rollout_year, lat, lon) %>% 
  arrange(community) %>% 
  # remove rows where community_area is NA
  # community_area is NA because stations are in Evanston, not Chicago
  filter(!is.na(community))

# convert data frame to sf object
stations_rollout <- st_as_sf(
  x = stations_rollout, 
  coords = c("lon", "lat"))

st_crs(stations_rollout)
st_crs(communities)
stations_rollout <- st_set_crs(stations_rollout, 4326)
communities <- st_set_crs(communities, 4326)

# use st_join() to join stations_rollout and communities
stations_rollout <- st_join(stations_rollout, left = FALSE, communities) %>% 
  mutate(
    community = community.x
  ) %>% 
  mutate(rollout_year_binned = factor(rollout_year, 
                                      levels = c(2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021), 
                                      labels = c("2013-2015", "2013-2015", "2016-2017", "2016-2017", 
                                                 "2018-2019", "2018-2019", "2020-2021", "2020-2021")
  )) %>% 
  dplyr::select(station, community, region, rollout_year, rollout_year_binned, geometry) 

# save
write_rds(stations_rollout, "data/stations_rollout.RDS")
