library(broom)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leafpop)
library(mapview)
library(rgdal)
library(sf)
library(shiny)
library(skimr)
library(spData)
library(thematic)
library(tidyverse)

source("app/theme.R")
source("communities.R")
source("mapview.R")


crimes_map_ui <- 
  fluidPage(
    plotOutput("plot")
  )

crimes_map_server <- function(input, output, session) {
  output$plot <- renderPlot({
    mapview(
      crimes_type_map, 
      zcol = "num_crimes", 
      popup = leafpop::popupTable(crimes_type_map, zcol = c("community", "num_crimes"))
    )
  })
}


ui <-
    navbarPage(
        theme = equiticitytheme,
        
        tabPanel("Divvy summary"),
        tabPanel("Divvy station rollout", "three"),
        navbarMenu("Violence and enforcement",
                 tabPanel("Gang territories", "map of gang territories"),
                 tabPanel("Crime by type", plotOutput("plot")),
                 tabPanel("panel 3c", "3c")
            ),
        navbarMenu("Demographics",
                   tabPanel("Census", "map of gang territories"),
                   tabPanel("panel 4b", "4b"),
                   tabPanel("panel 4c", "4c")
            ),
        tabPanel("About", "TEXT")
    )

server <- function(input, output, session) {
    thematic::thematic_shiny()

    output$plot <- renderPlot({
      map
    })
}
