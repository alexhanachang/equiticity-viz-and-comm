##################################################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#### set up
# load packages
library(broom)
library(bslib)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(leaflet)
library(leafpop)
library(mapview)
library(pointdexter)
# to download pointdexter^ type remotes::install_github("cenuno/pointdexter") in your console
#install the remotes package first
library(RColorBrewer)
library(readxl)
library(sf)
library(shiny)
library(thematic)
library(tidyverse)
library(viridis)

mapviewOptions(fgb = FALSE)

# read in data
census <- readRDS("data/census.RDS")
communities <- readRDS("data/communities.RDS")
divvy_demographics <- readRDS("data/divvy_demographics.RDS")
stations_rollout <- readRDS("data/stations_rollout.RDS")

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


communities <- read_rds("data/communities.RDS")
census <- read_rds("data/census.RDS")
relevant_crimes_community <- read_rds("data/relevant_crimes_community.RDS")
police_sentiment_data <- read_rds("data/police_sentiment_data.RDS")
crime_statistics <- read_rds("data/crime_statistics.RDS")

community_labs <- relevant_crimes_community$community


##################################################################
# define UI
ui <- shinyUI(
  navbarPage("Equiticity Data Dashboard", 
             tabPanel(
  # app title
  "Divvy Stations Rollout",
  
  # select year
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year",
                  choices = c("All", levels(stations_rollout$rollout_year)),
                  selected = "All", multiple = TRUE
      )
    ),

    # show map
    mainPanel(mapviewOutput("map")))), 
  tabPanel(
    "Violence and Enforcement Plots",
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
                  sidebarPanel(
                    # crimes input
                    p("The following lineplot shows the total number of outdoor/vehicle-related
            crimes for each year from 2013-2021."),
            selectInput(inputId = "community_input", 
                        label = "Select Community:",
                        choices = as.list(community_labs),
                        selected = 1),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("----------
            ----------
            ---------"),
            p("Crime rate is defined as the number of crimes per 100,000 people. 
          Divvy to population rate is defined as the number of Divvy
          stations per 100,000 people. Divvy to area rate is the number of Divvy stations 
          divided by the area in square miles. The Divvy to population map doesn't yield
          much correlation, but the Divvy per area reveals a lower Divvy per area rate in 
          southern neighborhoods with more crimes."),
          # crimerate input
          radioButtons(inputId = "crimerate_input", 
                       label = "Select Measure:",
                       choices = as.list(c("divvy_to_area",
                                           "divvy_to_pop")),
                       selected = "divvy_to_area"),
          # police sentiment
          p("----------
            ----------
            ---------"),
          p("----------
            ----------
            ---------"),
          p("----------
            ----------
            ---------"),
          p("----------
            ----------
            ---------"),
          p("----------
            ----------
            ---------"),
          p("----------
            ----------
            ---------"),
          p("Chicago Police Sentiment (2017-2021)"),
          p("Data can be found in the Chicago Data Portal. It is updated monthly and the data 
            owner is the Chicago Police Department, categorized under ‘Public Safety’. 
            Sorted by the 77 communities, this data is aggregated by year. 
            Each column gives either a trust or safety score for a demographic group, 
            or the average score across the entire community. The demographic categories 
            include age, sex, race, education, and income level. "),
          radioButtons("fill", 
                       label = "Select Trust or Safety Score for Fill",
                       choices = list("Trust Score", 
                                      "Safety Score"),
                       selected = "Trust Score"),
                  ),
          
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("crimes_timeline"),
            plotOutput("crimerate_map"),
            plotOutput("trust_safety_plot"),
            plotOutput("gangs_plots"),
            plotOutput("crashes_plot")
          )))
))
    

##################################################################
# define server
server <- function(input, output, session) {
  
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
    
    output$crimes_timeline <- renderPlot({
      
      # user-supplied community area
      # relevant_crimes_community_plot <- relevant_crimes_community %>% 
      #   filter(community == input$community_input)
      
      # building timeline plot
      relevant_crimes_community %>% 
        filter(community == input$community_input) %>% 
        ggplot(aes(x = year, y = num_crimes)) +
        geom_line(color = "#7F998C", size = 1) +
        geom_point(color = "#4A5952", size = 4) +
        labs(
          x = "Year",
          y = "Crimes",
          title = "Crimes in Chicago Over Time"
        )  +
        theme_minimal() +
        theme(text = element_text(family = "Arial", face = "bold", color = "#668C7A"), axis.text = element_text(color = "#6BA033")) +
        scale_x_continuous(breaks = seq(2013, 2021, 1))
      
    })
    
    output$crimerate_map <- renderPlot({
      
      # user-supplied input
      pop_or_area <- case_when(
        input$crimerate_input == "divvy_to_area" ~ pull(crime_statistics, divvy_to_area),
        input$crimerate_input == "divvy_to_pop" ~ pull(crime_statistics, divvy_to_pop),
      )
      
      crime_statistics %>% 
        ggplot() +
        geom_sf(mapping = aes(geometry = geometry, fill = pop_or_area)) +
        geom_point(mapping = aes(x = lat, y = lon, size = crime_rate), 
                   color = "red", alpha = 0.5) +
        theme_void() +
        labs(title = 
               "Crime Rate by Community") +
        theme(text = element_text(family = "Arial", face = "bold", color = "#668C7A")) +
        scale_fill_viridis()
      
    })
    
    # police sentiment plot
    output$trust_safety_plot <- renderPlot({
      
      # variable for fill
      fill_var <- switch(input$fill,
                         "Trust Score" = police_sentiment_data$trust, 
                         "Safety Score" = police_sentiment_data$safety
      )
      
      # creating the map
      crime_statistics %>% 
        filter(community != "All - Chicago") %>% 
        ggplot() +
        geom_sf(mapping = aes(geometry = geometry, fill = fill_var)) +
        theme_void() +
        labs(title = 
               "Police Sentiments by Community") +
        theme(text = element_text(family = "Arial", face = "bold", color = "#668C7A")) +
        scale_fill_viridis()
      
    })
    
}


##################################################################

# run app 
shinyApp(ui = ui, server = server)



