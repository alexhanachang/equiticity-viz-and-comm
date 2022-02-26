#### SET UP
# load packages
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

mapviewOptions(fgb = FALSE)

# read in data
stations_rollout <- readRDS("data/stations_rollout.RDS")

communities <- readRDS("data/communities.RDS")

# UI and server
source("mapview app.R")

##################################################################
# define UI
ui <- fluidPage(
  navbarPage(
   # theme = equiticitytheme,
    "PAGE TITLE",
    navbarMenu("Divvy stations rollout",
               tabPanel("Stations rollout", map_ui),
               tabPanel("panel 3c", "3c")
    ),
    navbarMenu("Divvy Station/Bike Prevalence by Community Demographics",
               tabPanel("3. Divvy Station/Bike Prevalence by Community Demographics", "map"),
               tabPanel("Divvy Station Prevalence by Community Demographics", "scatterplots"),
               tabPanel("5. Divvy Bike Prevalence by Community Demographics", "scatterplots")
    ),
    tabPanel("About", "TEXT")
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  map_server
}

shinyApp(ui,server)





##################################################################
# output$plot <- renderLeaflet({
#   mapview(
#     crimes_type_map, 
#     zcol = "num_crimes", 
#     popup = leafpop::popupTable(crimes_type_map, zcol = c("community", "num_crimes")))@map
# })



