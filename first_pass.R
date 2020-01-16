library(sf)
library(sp)
library(leaflet)
library(tidyverse)
precincts <- st_read("data/precincts/precincts_2.shp")
Voters <- read.csv("data/Voters.csv")

precincts %>%
  leaflet() %>% 
  addPolygons() %>%
  addTiles()

NH_Vote %>%
  leaflet() %>% 
  addPolygons() %>%
  addTiles()

leaflet() %>%
  addCircles(lat = Keene_voters$residence_addresses_latitude, lng  = Keene_voters$residence_addresses_longitude) %>%
  addTiles()

Voters %>%
  filter(residence_addresses_city == "Berlin" & van_precinct_id == 972208) %>%
  leaflet() %>%
  addCircles(lat = ~residence_addresses_latitude, lng =~residence_addresses_longitude) %>%
  addTiles()
