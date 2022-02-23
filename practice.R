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