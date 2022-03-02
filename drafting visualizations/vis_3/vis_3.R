library(broom)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leafpop)
library(mapview)
library(rgdal)
library(RColorBrewer)
library(sf)
library(shiny)
library(skimr)
library(spData)
library(thematic)
library(tidyverse)

divvy_demographics <- read_rds("data/divvy_demographics.RDS")

## race
race_distribution <- divvy_demographics %>% 
  pivot_longer(
    c(white, non_white), 
    names_to = "race", 
    values_to = "count"
  ) %>% 
  mutate(
    community = community %>% factor(),
    community_id = community_id %>% factor()
  ) %>% 
  dplyr::select(community_id, community, region, race, count) %>% 
  st_drop_geometry() %>% 
  tibble()

race_distribution %>% 
  filter(community_id == 1) %>% 
  ggplot(aes(fill = race, x = community, y = count)) + 
  geom_bar(position = "fill", stat = "identity") + 
  labs(title = income_distribution$community[1])

race_plots <- list()
for(i in levels(race_distribution$community_id)){
  plot <- race_distribution %>% 
    filter(community_id == i) %>% 
    ggplot(aes(fill = race, x = community, y = count)) + 
    geom_bar(position = "fill", stat = "identity") + 
    labs(title = income_distribution$community[i])
  race_plots[[i]] <- plot
}

mapview(divvy_demographics, zcol = "prop_white", popup = popupGraph(race_plots), col.regions = brewer.pal(6, "Blues"))


## income
income_distribution <- divvy_demographics %>% 
  pivot_longer(
    c(inc_lt_25k, inc_25_50k, inc_50_75k, inc_75_100k, inc_100_150k, inc_gt_150k), 
    names_to = "income", 
    values_to = "count"
  ) %>% 
  mutate(
    community = community %>% factor(), 
    community_id = community_id %>% factor()
  ) %>% 
  dplyr::select(community_id, community, region, income, count) %>% 
  st_drop_geometry() %>% 
  tibble()

income_distribution %>% 
  filter(community_id == 1) %>% 
  ggplot(aes(income, count)) + 
  geom_col() + 
  labs(title = income_distribution$community[1])

income_plots <- list()
for(i in levels(income_distribution$community_id)){
  plot <- income_distribution %>% 
    filter(community_id == i) %>% 
    ggplot(aes(income, count)) + 
    geom_col() + 
    labs(title = income_distribution$community[i])
  income_plots[[i]] <- plot
}

mapview(
  divvy_demographics, 
  zcol = "msh_index_score",
  popup = popupGraph(income_plots),
  col.regions = brewer.pal(6, "Blues")
  )


