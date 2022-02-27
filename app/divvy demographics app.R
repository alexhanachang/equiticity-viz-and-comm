# load packages
library(mapview)
library(leaflet)
library(sf)
library(shiny)
library(tidyverse)

mapviewOptions(fgb = FALSE)

# read in data
stations_rollout <- readRDS("data/stations_rollout.RDS") %>% 
  mutate(rollout_year = rollout_year %>% factor())

communities <- readRDS("data/communities.RDS")


##################################################################
# define UI
rollout_map_ui <- fluidPage(
  
  # app title
  titlePanel("Divvy Stations Rollout"),
  
  # select year
  sidebarLayout(
    sidebarPanel(
      selectInput("fill_var", "Fill variable",
                  choices = c("All", levels(stations_rollout$rollout_year)),
                  selected = "All", multiple = FALSE
      )
    ),
    
    # show map
    mainPanel(
      mapviewOutput("map")
    )
  )
)

# define server
rollout_map_server <- function(input, output) {
  
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
      zcol = "region"
    ) + 
        mapview(
          filter_year, 
          xcol = "lon", ycol = "lat", 
          zcol = "rollout_year", 
          grid = FALSE
        )
    )@map
  })
}

# run app 
shinyApp(ui = rollout_map_ui, server = rollout_map_server)


