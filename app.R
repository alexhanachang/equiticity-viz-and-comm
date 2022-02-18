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


communities <- readRDS('data/communities.RDS')
crimes <- read_csv('data/violence_enforcement/crimes_2021.csv')

crimes_type <- crimes %>% 
  mutate(community_id = as.numeric(community_area)) %>% 
  group_by(community_id, primary_type) %>% 
  summarise(num_crimes = n()) %>% 
  filter(num_crimes == max(num_crimes))

crimes_type_map <- communities %>% 
  right_join(crimes_type, by = c('community_id' = 'community_id')) %>% 
  select(community_id, community, primary_type, num_crimes, geometry)

mapviewOptions(fgb = FALSE)

ui <- fluidPage(
  navbarPage(
    theme = equiticitytheme,
    
    tabPanel("Divvy summary"),
    tabPanel("Divvy station rollout", "three"),
    navbarMenu("Violence and enforcement",
               tabPanel("Gang territories", "map of gang territories"),
               tabPanel("panel 3c", "3c")
    ),
    navbarMenu("Demographics",
               tabPanel("Census", "map of gang territories"),
               tabPanel("panel 4b", "4b"),
               tabPanel("panel 4c", "4c")
    ),
    tabPanel("About", "TEXT")
  ),
  
    fillPage(
      leafletOutput("plot")
    )
  )

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  output$plot <- renderLeaflet({
    mapview(
      crimes_type_map, 
      zcol = "num_crimes", 
      popup = leafpop::popupTable(crimes_type_map, zcol = c("community", "num_crimes")))@map
  })
}

shinyApp(ui,server)


