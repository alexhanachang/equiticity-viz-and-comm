#### SET UP
# load packages
library(gganimate)
library(mapview)
library(RColorBrewer)
library(sf)
library(tidyverse)
library(viridis)

mapviewOptions(fgb = FALSE)

# read in data
stations_rollout <- readRDS("data/stations_rollout.RDS") 
communities <- readRDS("data/communities.RDS")


##################################################################
## stations rollout by year
# creating a function to filter by year and map
a <- stations_rollout %>% 
  filter(stations_rollout$rollout_year[] == 2021)

mapview(communities) + 
  mapview(
    a, 
    xcol = "lon", 
    ycol = "lat", 
    grid = FALSE
  )

func <- function(x){
  b <- stations_rollout %>% filter(stations_rollout$rollout_year[] == x)
  print(
    mapview(communities) + 
      mapview(
        b, 
        xcol = "lon", 
        ycol = "lat", 
        grid = FALSE, 
        zcol = "rollout_year"
      )
  )
}

func(2021)

# map stations by region
mapview(
  communities, 
  legend = FALSE
  ) +
  mapview(
    (stations_rollout %>% filter(region == "Far Southwest Side")), 
    zcol = "rollout_year_binned", 
    legend = TRUE, 
    layer.name = "Rollout year"
  )

mapview(
  communities, 
  legend = FALSE
  ) +
  mapview(
    (stations_rollout %>% filter(rollout_year == "2013")), 
    zcol = "region", 
    legend = TRUE, 
    layer.name = "Regions", 
    col.regions = brewer.pal(5, "RdPu")
  )

mapview(
  communities,
  zcol = "region",  
  legend = FALSE,
  col.regions = brewer.pal(9, "Greys")
  ) +
  mapview(
    rollout_regions, 
    zcol = "rollout_year_binned", 
    legend = TRUE, 
    layer.name = "Rollout year", 
    col.regions = brewer.pal(4,"RdPu"),
    cex = 3
  )

mapview(stations_rollout)


# ridgeline plot
library(ggridges)
library(readxl)

stations_rollout_ridge <- read_excel("drafting visualizations/vis_1/Full Network to Date Station Install Dates - 2_1_22.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(
    community = (community_area %>% str_to_title()),
    station = name,
    rollout_year = format(as.Date(install_date, format="%Y/%m/%d"),"%Y", ),
    rollout_month = format(as.Date(install_date, format="%Y/%m/%d"),"%m", )
  ) %>% 
  filter(!is.na(community)) 

stations_rollout_ridge[stations_rollout_ridge$community == "Mckinley Park", "community"] <- "McKinley Park"

stations_rollout_ridge <- stations_rollout_ridge %>% 
  left_join(communities, by = c("community" = "community")) %>% 
  dplyr::select(station, community, region, rollout_year)

stations_rollout_ridge <- stations_rollout_ridge %>% 
  group_by(region, rollout_year) %>% 
  summarise(new_stations = n()) %>% 
  mutate(rollout_year = rollout_year %>% as.numeric())
# set breaks

ggplot(stations_rollout_ridge, aes(x = rollout_year, y = region)) + 
  geom_density_ridges() + 
  theme_ridges() 





