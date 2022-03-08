# load packages
library(mapview)
library(leaflet)
library(RColorBrewer)
library(sf)
library(shiny)
library(tidyverse)

mapviewOptions(fgb = FALSE)

# read in data
stations_rollout <- readRDS("data/stations_rollout.RDS") %>% 
  mutate(
    rollout_year = rollout_year %>% factor(),
    region = region %>% factor(),
    community = community %>% factor()
  ) %>% 
  mutate(region = fct_relevel(
    region, "Far North Side", "Northwest Side", "North Side", "West Side", "Central", 
    "Southwest Side", "South Side", "Far Southwest Side", "Far Southeast Side")
  )

communities <- readRDS("data/communities.RDS") %>% 
  mutate(region = fct_relevel(
    region, "Far North Side", "Northwest Side", "North Side", "West Side", "Central", 
    "Southwest Side", "South Side", "Far Southwest Side", "Far Southeast Side")
  )

##################################################################
# define UI
ui <- fluidPage(
  
  # app title
  titlePanel("Divvy Stations Rollout"),
  
  # select year
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year",
                  choices = c("All", levels(stations_rollout$rollout_year)),
                  selected = "All", multiple = TRUE
      )
    ),
    
    # show map
      mapviewOutput("map")
  )
)

# define server
server <- function(input, output) {
  
  # reactive input/output
  map_year <- reactive({
    filter_year <- stations_rollout
    if(input$year != "All") filter_year <-
        filter_year <- stations_rollout[stations_rollout$rollout_year == input$year, ] 
    filter_year
  })
  
  # output
  output$map <- renderLeaflet({
    
    # get data
    filter_year <- map_year()
    
    # generate map
    (mapview(
        communities, 
        zcol = "region", 
        col.regions = brewer.pal(9, "Blues")
      ) + 
      mapview(
        filter_year, 
        xcol = "lon", ycol = "lat", 
        zcol = "rollout_year", 
        layer.name = "Year",
        grid = FALSE,
        col.regions = brewer.pal(8, "RdPu"), 
        cex = 3
      )
    )@map
  })
}

# run app 
shinyApp(ui = ui, server = server)


