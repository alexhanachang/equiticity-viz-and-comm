

## SHINY APP THEME
library(bslib)
library(dplyr)
library(extrafont)
library(ggplot2)
library(janitor)
library(shiny)
library(showtext)
library(thematic)
library(tidyverse)

#### DIVVY THEME ####
divvytheme <- bslib::bs_theme(
  bg = "#FFFFFF", 
  fg = "#000000", 
  primary = "#5FB3E0", 
  secondary = "#A2A6F2", 
  base_font = font_google("Maven Pro"),
  heading_font = font_google("Maven Pro")
)

bslib::bs_theme_preview(divvytheme)

## colors for divvy theme:
# "#5FB3E0"
# "#C5D0D9"
# "#A2A6F2"

# divvy theme fonts
# family = "Maven Pro", face = "bold"



#### EQUITICITY THEME ####
equiticitytheme <- bslib::bs_theme(
  bg = "#FFFFFF", 
  fg = "#000000", 
  primary = "#668C7A", 
  secondary = "#8C5F58", 
  success = "#6BA033", 
  info = "#EDFFF6", 
  warning = "#B4D9C7",
  danger = "#7F998C"
)

bslib::bs_theme_preview(equiticitytheme)

## colors used for equiticity theme:
# "#668C7A" (sage)
# "#8C5F58" (brown)
# "#6BA033" (grass green)
# "#EDFFF6" (pale green)
# "#B4D9C7" (darker mint)
# "#7F998C" (olive)

## colors NOT used for equiticity theme: 
# "#BEE6D3" (mint)
# "#9FBFB0" (sage)
# "#4A5952" (dark olive)

## equiticity theme fonts
# family = "Arial", face = "bold"

