# load packages
library(mapview)
library(leaflet)
library(sf)
library(shiny)

mapviewOptions(fgb = FALSE)

# read in data
stations_rollout <- readRDS("data/stations_rollout.RDS") %>% 
  mutate(rollout_year = rollout_year %>% factor())

communities <- readRDS("data/communities.RDS")


##################################################################
# define UI
map_ui <- fluidPage(
  
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
    mainPanel(
      mapviewOutput("map")
    )
  )
)

# define server
map_server <- function(input, output) {
  
  fran <- reactive({
    f <- stations_rollout
    if(input$year != "All") f <-
        f <- stations_rollout[stations_rollout$rollout_year == input$year, ] 
    
    f
  })
  
  output$map <- renderLeaflet({
    
    # get data
    f <- fran()
    
    # generate map
    (mapview(communities, zcol = "region") + mapview(f, 
                                                     xcol = "lon", ycol = "lat", 
                                                     zcol = "rollout_year", grid = FALSE))@map
  })
}

# run app 
shinyApp(ui = ui, server = server)


