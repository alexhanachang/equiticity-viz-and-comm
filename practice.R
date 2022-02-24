library(shiny)
library(leaflet)
library(xts)
library(dplyr)

stations_rollout <- readRDS("data/stations_rollout.RDS")
library(lubridate)

stations_rollout %>% 
  mutate(rollout_year = stations_rollout$rollout_year %>% as.numeric() %>% lubridate::year())
communities <- readRDS("data/communities.RDS")

library(mapview)
library(gganimate)
library(move)
library(moveVis)
library(viridis)

stations_rollout <- stations_rollout %>% 
  mutate(install_year = stations_rollout$install_date %>% year())

stations_rollout <- read_excel("drafting visualizations/vis_1/Full Network to Date Station Install Dates - 2_1_22.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(
    community = (community_area %>% str_to_title()),
    station = name#,
 #   rollout_year = year(rollout_year)
  ) %>% 
 # select(station, community, rollout_month, rollout_year, lat, lon) %>% 
  arrange(community) %>% 
  # remove rows where community_area is NA
  # community_area is NA because stations are in Evanston, not Chicago
  filter(!is.na(community))

pal <- magma(n = length(unique(stations_rollout$rollout_year)))

mapview(communities, legend = FALSE) + 
  mapview(stations_rollout, xcol = "lon", ycol = "lat", zcol = "install_year", col.regions = pal)
  
date<-seq(as.Date("2015-01-01"), as.Date("2015-01-10"), by="day")
a<-xts(1:10,order.by=date)
df = data.frame(Lat = rnorm(1)+10, Long = rnorm(1),Id=a)

data_a<-data.frame(a)
data_a1<-data_a %>%  
  mutate("Lat" =as.numeric(df[1,1]),"Long"=as.numeric(df[2,1]),"Date"=rownames(data_a))

ui <- fluidPage(
  sliderInput("year", "year", min(stations_rollout$install_year), 
              max(stations_rollout$install_year),
              value = max(stations_rollout$install_year),
              step=1,
              animate=T),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  points <- reactive({
    stations_rollout %>% 
      filter(Date==input$year)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addMarkers(data = points())
    
    mapview(communities, legend = FALSE) + 
      mapview(stations_rollout, xcol = "lon", ycol = "lat", zcol = "install_year", col.regions = pal)
  })
}

library(htmltools)

a <- stations_rollout %>% 
  filter(install_year == 2013)

leaflet(communities) %>% 
  addTiles() %>% 
  addMarkers(
    lng = a$lon, 
    lat = a$lat, 
    label = htmlEscape(a$name))

b <- stations_rollout %>% 
  filter(install_year == 2015)

leaflet(communities) %>% 
  addTiles() %>% 
  addMarkers(
    lng = b$lon, 
    lat = b$lat, 
    label = htmlEscape(b$name))
            
shinyApp(ui, server)



pal <- magma(n = length(unique(stations_rollout$rollout_year)))

mapview(communities, legend = FALSE) + 
  mapview(stations_rollout, xcol = "lon", ycol = "lat", zcol = "rollout_year", col.regions = pal)



typeof(stations_rollout)
as.data.frame(stations_rollout)

stations_rollout %>% 
  select(unique(stations_rollout$install_date))

stations_rollout <- distinct(stations_rollout, install_date, .keep_all = T)

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
