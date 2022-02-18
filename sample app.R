library(broom)
library(bslib)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sf)
library(shiny)
library(thematic)
library(tidyverse)


neighborhood_map <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")


ui <- navbarPage(
  "Sample app",
  titlePanel("Flight Delay Report"),
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput("community_area", "Community area:", 
                  choices = neighborhood_map$community_area,
                  selected = 1
      )
    )
  ),
  mainPanel = mainPanel(
    h3("Average Departure Delay"),
    plotOutput("plot")
  )
  )

server <- function(input, output, session) {
  
    output$plot <- renderPlot({
        neighborhood_map %>%
            filter(community == input$community_area) %>%
            ggplot() +
                geom_sf() +
                coord_sf()
    })
}


shinyApp(ui, server)
