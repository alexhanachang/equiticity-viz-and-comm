#### SET UP
# load packages
library(gganimate)
library(mapview)
library(move)
library(moveVis)
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
