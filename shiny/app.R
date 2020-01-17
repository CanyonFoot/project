## Libraries must be called using `library()` when deploying shiny apps
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(rio)
library(sp)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)
library(scales)
library(viridis)
library(shinydashboard)
library(Cairo)
library(rstudioapi)
library(here)

# Shapefile for precincts
geo_map_pre <- st_read("data/precincts/precincts.shp")
# Shapefile for fo
geo_map_fo <- st_read("data/fo_shapefile/fo_shapefile.shp")
# Shapefile for region
geo_map_region <- st_read("data/region_shapefile/region_shapefile.shp")
# Translation file 
geo_data <- read_csv("data/full_geo_ids")
# csv with Support_id
survey <- read_csv("data/sample_vfact_contactcontact.csv")
# csv with Demographic info and location
respondents <- read_csv("data/voters.csv")
# Volunteers csv
volunteers <- read_csv("data/volunteers_imputed.csv") %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(level = as.numeric(str_sub(volunteer_level, -1,-1))) %>%
  select(latitude, longitude, van_precinct_id, level) %>%
  left_join(geo_data)



respondents <- respondents %>%
  select(person_id, van_precinct_id)

survey_filter <- survey %>%
  mutate(person_id = as.numeric(person_id)) %>%
  full_join(respondents) %>%
  filter(unique_id_flag == TRUE & supportid != 0) %>%
  select(supportid, person_id)


survey_voters_geo <- inner_join(survey_filter, respondents) %>%
  inner_join(geo_data) 

support_map_pre <- survey_voters_geo %>%
  group_by(van_precinct_id) %>%
  summarise(total_count = n(), 
            sup_count = sum(supportid == 1), 
            sup_prop = mean(supportid == 1)) %>%
  full_join(geo_data) %>%
  select(total_count, sup_count, sup_prop, van_precinct_id, voting_dis)
  


support_map_fo <- survey_voters_geo %>%
  group_by(fo_id) %>%
  summarise(total_count = n(),
            sup_count = sum(supportid == 1), 
            sup_prop = mean(supportid == 1)) %>%
  full_join(geo_data) %>%
  select(total_count, sup_count, sup_prop, fo_id, van_precinct_id)

support_map_region <- survey_voters_geo %>%
  group_by(region_id) %>%
  summarise(total_count = n(), 
            sup_count = sum(supportid == 1), 
            sup_prop = mean(supportid == 1)) %>%
  full_join(geo_data) %>%
  select(total_count, sup_count, sup_prop, region_id)

geo_map_pre_final <- geo_join(geo_map_pre, support_map_pre,
                              by_df = "voting_dis", by_sp = "voting_dis") %>%
  mutate(select_id = van_precinct_id) %>%
  select(select_id, van_precinct_id, sup_prop, sup_count)


geo_map_fo_final <- geo_map_fo  %>%
  inner_join(support_map_fo, by = c("van_precin" = "van_precinct_id")) %>%
  mutate(select_id = fo_id) %>% 
  select(select_id, fo_id, sup_prop, sup_count)

geo_map_region_final <- geo_join(geo_map_region, support_map_region,
                                 by_df = "region_id", by_sp = "region_nam") %>%
  mutate(select_id = region_nam) %>%
  select(select_id, region_nam, sup_prop, sup_count)


tmap_mode("view")


### FIRST TAB 

# Shiny uses (fluid) pages, (fluid) rows, and columns to organize each tab. In general, pages contain rows and 
# rows contain columns. The first argument taken by `column()` is width, which defines how much of the row
# the content of the column is to take up. A row can contain multiple columns, but the widths of the columns
# corresponding to a particular row must sum to 12 or less. So a column with width = 6 will take up half of the
# row horazontally.

# Each output requires an outputId. I have defined the outputId for the typology Map as "typologyMap" in the call
# output$typologyMap <- renderLeaflet({ ... }). These Ids must be defined in this way, and are defined in the 
# server portion of the app. This means that outputs can adapt to user input, which is the heart of interactivity 
# in Shiny. More on this later.

tab1 <-  fluidPage(fluidRow(
  # Left part of first row is typology map
  column(width = 6, leafletOutput(outputId = "bernie_map"), height = "80%"),
  column(width = 6, plotlyOutput("sup_plot")))

  )



### APP BODY - CONTAINS BOTH TABS

## Creates the three tabs using UIs defined above
body <- dashboardBody(tab1
)


### HEADER

header <- dashboardHeader(title = "Test")



### SIDE BAR 


sidebar <- dashboardSidebar(radioButtons(inputId = "geometry_select", 
                                                            h3("Select Geographic Level"), 
                                                            choices = list("Precincts" = 'pre', 
                                                                           "Field Office" = 'fo',
                                                                           "Region" = "region"), 
                                                            selected = 'pre'),
                            checkboxGroupInput("level_select", "Select Volunteer Levels",
                                               c("0" = "0",
                                                 "1" = "1",
                                                 "2" = "2",
                                                 "3" = "3",
                                                 "4" = "4"), selected = c("1", "2", "3", "4"))
                                        
)

### FINAL UI CALL, puts together the parts

ui <- dashboardPage(
  header,
  sidebar,
  body
)

get_map <- function(selected_geo) {
  if (selected_geo == "pre") {
    geo_map_pre_final
  } else if (selected_geo == "fo") {
    geo_map_fo_final
  } else if (selected_geo == "region") {
    geo_map_region_final
  }
}

get_vol <- function(selected_geo, id, selected_levels) { 
  if (selected_geo == "pre") {
    volunteers %>%
      filter(van_precinct_id == id & level %in% selected_levels)
  } else if (selected_geo == "fo") {
    volunteers %>%
      filter(fo_id == id & level %in% selected_levels)
  } else if (selected_geo == "region") {
    volunteers %>%
      filter(region_id == id & level %in% selected_levels)
  }
}


server <- function(input, output, session) {

  ## use reactive values to store the data you generate from observing the shape click
  # rv <- reactiveValues()
  
  selected_van_id <- reactiveVal(value = 972421)
  geo_map <- reactiveVal(value = geo_map_pre_final)

  
  pal <- colorNumeric(palette="viridis", domain=geo_map_pre_final$sup_prop, na.color="transparent")

    
  output$bernie_map <- renderLeaflet({
      get_map(input$geometry_select) %>%
      leaflet() %>%
      addPolygons(
        fillColor  = ~pal(sup_prop),
        weight = .25,
        opacity = 1,
        fillOpacity = .7,
        layerId = ~select_id
      ) %>%
      addLegend(pal = pal, values = ~sup_prop, opacity = 0.7, title = NULL,
                  position = "bottomright") %>%
      addProviderTiles(providers$Hydda)
  
  })
  

  
  observeEvent(input$bernie_map_shape_click, { # update the selected geoid on map clicks
    leafletProxy("bernie_map") %>%
      clearGroup("volunteers") %>%
      addMarkers(data = get_vol(input$geometry_select, input$bernie_map_shape_click$id, as.numeric(input$level_select)),
                 ~longitude,
                 ~latitude,
                 group = "volunteers")
    
    output$sup_plot <- renderPlotly({
      sup_plot <- survey_voters_geo %>%
        filter(van_precinct_id == selected_van_id()) %>%
        ggplot(aes(x = supportid)) +
        geom_bar()
      ggplotly()
    })
    
  })  

  
}





shinyApp(ui, server, options = list(height = 1080))
