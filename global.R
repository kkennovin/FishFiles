library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(leaflet)

fish = read.csv('fish2.csv', header = T, stringsAsFactors = F)

#fish3 = fish[1:50000,]
#write_csv(fish3, 'fish3.csv')

#shiny app link!
# https://katkennovin.shinyapps.io/shiny_project/