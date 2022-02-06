library(broom)
library(bslib)
library(dplyr)
library(ggplot2)
library(rgdal)
library(shiny)
library(thematic)
library(tidyverse)

bikeRouteDataFort <- read_rds("census team data/Bike Routes/bikeRouteDataFort.RDS")
roadsDataChi <- read_rds("census team data/Bike Routes/roadsDataChi.RDS")

map <- ggplot() +
    geom_path(data = roadsDataChi,
              aes(x = long, y = lat, group = group),
              color = "red") +
    geom_path(data = bikeRouteDataFort,
              aes(x = long, y = lat, group = group),
              color = "green")  +
    theme_void() + 
    labs(title = "Bike routes + roads in Chicago") + 
    theme(title = element_text(color = "white"))

ui <-
    navbarPage(
        theme = bslib::bs_theme(bootswatch = "darkly"), 
        "Equiticity",   
        tabPanel("map", plotOutput("plot")),
        tabPanel("panel 3", "three"),
        navbarMenu("subpanels", 
                   tabPanel("panel 4a", "four-a"),
                   tabPanel("panel 4b", "four-b"),
                   tabPanel("panel 4c", "four-c")
        ), 
        tabPanel("About", "TEXT")
    ) %>% 
    
    fluidPage(
    )


server <- function(input, output, session) {
    thematic::thematic_shiny()
    
    output$plot <- renderPlot({map})
    
}


shinyApp(ui, server)
