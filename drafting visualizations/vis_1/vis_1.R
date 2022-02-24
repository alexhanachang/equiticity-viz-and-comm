stations_rollout <- readRDS("data/stations_rollout.RDS") 
communities <- readRDS("data/communities.RDS")

library(gganimate)
library(mapview)
library(move)
library(moveVis)
library(tidyverse)
library(viridis)

mapviewOptions(fgb = FALSE)


library(RColorBrewer)


mapview(communities) +
  mapview(
  (rollout_region %>%
     filter(region == "Far Southwest Side")), 
  zcol = "rollout_year", 
  legend = TRUE, 
  layer.name = "x", 
  col.regions = brewer.pal(5, "RdPu")
    )

mapview(communities) +
  mapview(
    (rollout_region %>%
       filter(rollout_year == "2013")), 
    zcol = "region", 
    legend = TRUE, 
    layer.name = "x", 
    col.regions = brewer.pal(5, "RdPu")
  )

mapview(communities, zcol = "region",     
        col.regions = brewer.pal(9,"Greys")) +
  mapview(rollout_region, 
          zcol = "rollout_year_binned", 
    legend = TRUE, 
    layer.name = "x", 
    col.regions = brewer.pal(4,"RdPu"),
    cex = 3
  )

stations_rollout$rollout_year %>% unique()

rollout_region <- rollout_region %>% 
  mutate(rollout_year_binned = factor(rollout_year, 
         levels = c(2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021), 
         labels = c("2013-2015", "2013-2015", "2016-2017", "2016-2017", 
                    "2018-2019", "2018-2019", "2020-2021", "2020-2021")
  ))

rollout_region <- st_join(stations_rollout, left = FALSE, communities)


stations_rollout %>% add_column(region = NA)

isd_ca_co_pts <- st_join(isd_history, left = FALSE, ca_co["name"]) # join points
st_join(stations_rollout, left = FALSE, communities["community"])
st_join(communities, left = FALSE, stations_rollout)


# plot
plot(isd_ca_co_pts$geometry, pch=21, cex=0.7, col="purple", bg="gray80")
plot(ca_co$geometry, border="gray20", col=NA, add=T)


plot(stations_rollout$geometry)

### creating a function to filter by year
stations_rollout <- stations_rollout %>% 
  mutate(rollout_year = rollout_year %>% as.numeric())

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


### creating a function to filter by region
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



# Convert data frame to sf object
stations_rollout <- st_as_sf(
  x = stations_rollout, 
  coords = c("lon", "lat"))

st_crs(stations_rollout)

st_crs(communities)

communities <- st_set_crs(communities, 4326)
stations_rollout <- st_set_crs(stations_rollout, 4326)







