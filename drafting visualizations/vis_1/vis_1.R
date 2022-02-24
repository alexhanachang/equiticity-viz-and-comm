stations_rollout <- readRDS("data/stations_rollout.RDS")
communities <- readRDS("data/communities.RDS")

library(mapview)
library(gganimate)
library(move)
library(moveVis)
library(viridis)


pal <- magma(n = length(unique(stations_rollout$rollout_year)))

mapview(communities, legend = FALSE) + 
  mapview(stations_rollout, xcol = "lon", ycol = "lat", zcol = "rollout_year", col.regions = pal)



typeof(stations_rollout)
as.data.frame(stations_rollout)

stations_rollout %>% 
  select(unique(stations_rollout$install_date))

stations_rollout <- distinct(stations_rollout, install_date, .keep_all = T)



##################

test <- df2move(df = stations_rollout,
                proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                x = "lon", y = "lat", time = "install_date")

y <- align_move(test, unit = "days") %>% 
  head(10)

frames <- frames_spatial(y)
frames <- frames %>% 
  add_gg(frames, gg = expr(geom_path(aes(x = x, y = y), 
                                     colour = "blue", linetype = "dashed")))


animate_frames(frames, out_file = "t.gif")



data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "days")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()


# animate frames
animate_frames(frames, out_file = "moveVis.gif")

library(magrittr)
library(tibble)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(lubridate)
library(leaflet.extras2)

# how many test data points to create
num_points <- 100
df <- tibble::tibble(temp = (1:num_points),
                     lat = seq(from = 45, to = 46, length.out = num_points) + .1*sin(temp),
                     lon = seq(from = -75, to = -75.5, length.out = num_points) + .1*cos(temp),
                     datetime = seq(from = lubridate::ymd_hms("2021-09-01 8:00:00"),
                                    to = lubridate::ymd_hms("2021-09-01 9:00:00"),
                                    length.out = num_points)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE)

leaflet() %>%
  addTiles() %>%
  leaflet.extras2::addPlayback(data = df,
                               time = "datetime",
                               options = leaflet.extras2::playbackOptions(speed = 100))

leaflet() %>%
  addTiles() %>%
  leaflet.extras2::addPlayback(data = station_rollouts,
                               time = "install_date",
                               options = leaflet.extras2::playbackOptions(speed = 100))

test <- st_as_sf(station_rollouts, coords = c("lon", "lat"), 
                 crs = 4326, agr = "constant")
