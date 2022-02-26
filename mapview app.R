library(shiny)
library(mapview)
library(tidyverse)
library(leaflet)


stations_rollout <- readRDS("data/stations_rollout.RDS") %>% 
  mutate(rollout_year = rollout_year %>% as.numeric())
communities <- readRDS("data/communities.RDS")


mapviewOptions(fgb = FALSE)


ui <- fluidPage(
  # application title
  titlePanel("Rollout map"),
  
  # sidebar with a slider input for number of bins 
  sidebarLayout(
    
    # position sidebar
   mainPanel(),
     position = "right", 
   
   # radio buttons for fill variable
      radioButtons(
        inputId = "year",
        label = "Select year",
        choices = list(
          2013, 2014, 2015
        ),
        selected = 2015
      )
    ),
    
    # show plot
    mainPanel(
      leafletOutput("distPlot")
    )
  )


server <- function(input, output) {
  
  
  output$distPlot <- renderLeaflet({
    
    
    # fill_hist <- bindEvent(
    #   input$fill_var, 
    #   "2013" = (stations_rollout %>% filter(stations_rollout$rollout_year[] == 2013)), 
    #   "2014" = (stations_rollout %>% filter(stations_rollout$rollout_year[] == 2014)), 
    #   "2015" = (stations_rollout %>% filter(stations_rollout$rollout_year[] == 2015))
    # )
    
    # # building histogram
    # if(is.null(fill_hist)){
    #   
    # }


    (mapview(communities) + 
      mapview(
            stations_rollout %>% filter(stations_rollout$rollout_year[] == 2013), 
            xcol = "lon", 
            ycol = "lat", 
            grid = FALSE
          ))@map
  })

}

shinyApp(ui, server)





