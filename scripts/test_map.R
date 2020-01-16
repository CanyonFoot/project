library(sp)
library(sf)
library(tidyverse)
library(leaflet)
library(tmap)
library(tmaptools)
library(maptools)
nh_map <- st_read("data/precincts/precincts.shp")
support <- read_csv("data/geo_support.csv")
volunteers <- read_csv("data/volunteers_imputed.csv")
volunteers <- volunteers %>%
  filter(!is.na(latitude) & !is.na(longitude) & state == "NH") 

full_map <- full_join(nh_map, support)

unionSpatialPolygons(full_map,full_map$region_id) 


tmap_leaflet(tm_shape(full_map) + 
               
               # Specifying the 'col' arguement determines the chrolopleth variable on the map. Here
               # I am combining the radio button input with the variable input to give the specific
               # year/variable as the mapping
               tm_fill(col = 'sup_prop', 
                       alpha = 0.5,
                       
                       # This is an awkward work around of the R if_else syntax which requires that condiontions
                       # and output values be of the same length.
                       palette = c('#f3c058', '#ed8526', '#d3593f', 
                                   '#6f292f', '#575a8b', '#5f82b2', '#282f59'),
                       colorNA = '#e0e0e0', 
                       
                       
                       # sets 7 break points in data
                       n = 7,
                       # Uses quantiles instead of even length cut points - better for data that isn't 
                       # uniform
                       
                       showNA = T,
                       
                    
                       position = c("left","bottom")) +
               tm_polygons(lwd = 1) +
               tm_basemap(leaflet::providers$Hydda)) %>%
  addCircleMarkers(lat = volunteers$latitude, 
                   lng = volunteers$longitude, 
                   clusterOptions = markerClusterOptions(maxClusterRadius = 30),
                   clusterId = volunteers$van_precinct_id)



