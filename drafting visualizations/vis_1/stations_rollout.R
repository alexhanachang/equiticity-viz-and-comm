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

write_rds(stations_rollout, "data/stations_rollout.RDS")
