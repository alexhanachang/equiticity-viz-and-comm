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
    stations_rollout, 
    zcol = "rollout_year_binned", 
    legend = TRUE, 
    layer.name = "Rollout year", 
    col.regions = brewer.pal(4,"RdPu"),
    cex = 3
  )

mapview(stations_rollout)

stations_rollout_2013_2015 <- stations_rollout %>% 
  filter(rollout_year_binned == "2013-2015")

stations_rollout_2020_2021 <- stations_rollout %>% 
  filter(rollout_year_binned == "2020-2021")

mapview(communities) + mapview(stations_rollout)


map_2013_2015 <- mapview(communities) + mapview(stations_rollout_2013_2015, zcol = "region", 
                   col.regions = RColorBrewer::brewer.pal(9, "Greens"), alpha.regions = 1)

map_2020_2021 <- mapview(communities) + mapview(stations_rollout_2020_2021, zcol = "region", 
                   col.regions = RColorBrewer::brewer.pal(9, "Greens"), alpha.regions = 1)

library(leafsync)
sync(map_2013_2015, map_2020_2021)

map_2013_2015 | map_2020_2021
