library(broom)
library(bslib)
library(dplyr)
library(ggplot2)
library(rgdal)
library(shiny)
library(thematic)
library(tidyverse)

read_rds("census team data/Bike Routes/bikeRouteDataFort.RDS")
read_rds("census team data/Bike Routes/roadsDataChi.RDS")

ui <-
    navbarPage(
        theme = bslib::bs_theme(bootswatch = "darkly"), 
        "Equiticity",   
        # tabPanel("map", mainPanel(plotOutput("plot"))),
        tabPanel("demographics charts", "testing"),
        tabPanel("panel 3", "three"),
        navbarMenu("subpanels", 
                   tabPanel("panel 4a", "four-a"),
                   tabPanel("panel 4b", "four-b"),
                   tabPanel("panel 4c", "four-c")
        ), 
        tabPanel("About", "TEXT")
    ) %>% 
    
    fluidPage(
        titlePanel("Divvy stations in Chicago"),
        sidebarLayout(
            sidebarPanel(
                numericInput("m", "Number of samples:", 2, min = 1, max = 100)
            )
        )
    )


server <- function(input, output, session) {
    thematic::thematic_shiny()
    
    output$plot <- renderPlot({
        ggplot() +
            geom_path(data = roadsDataChi,
                      aes(x = long, y = lat, group = group),
                      color = "red") +
            geom_path(data = bikeRouteDataFort,
                      aes(x = long, y = lat, group = group),
                      color = "green") +
            theme_void()
    })
    
}


shinyApp(ui, server)
