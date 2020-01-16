library(sqldf)
library(tidyverse)
library(sf)
library(sp)
van_data <- read_csv("data/vanNHprecincts.csv") 
van_data <- van_data %>%
  select(precinct_name, van_precinct_id, van_precinct_name)

van_data$van_precinct_name <- toupper(van_data$van_precinct_name)

geo_data <- st_read("data/precincts/precincts_2.shp") 
geo_data$geometry <- NULL

geo_data <- geo_data %>%
  select(voting_dis, voting_d_2)

### PREPARE geo_data

geo_data$census_lsa <- str_replace(geo_data$census_lsa, "GOFFSTOWN WARD 2", "GOFFSTOWN WARD 5")
geo_data$voting_d_2 <- str_replace(geo_data$voting_d_2, "GOFFSTOWN WARD 2", "GOFFSTOWN WARD 5")
geo_data$voting_dis <- str_replace(geo_data$voting_dis, "GOFF02", "GOFF05")


edit1 <- str_remove(geo_data$voting_d_2, "TOWNSHIP OF ")
edit2 <- str_remove(edit1, "TOWN OF ")


geo_data$voting_district_name_simple <- edit2

geo_data <- geo_data %>%
  mutate(ward_num = as.numeric(str_sub(voting_district_name_simple, -2, -1)),
         ward_num = if_else(is.na(ward_num), 100, ward_num)) %>%
  mutate(voting_district_name_simple = if_else(ward_num < 10, 
                                               str_replace(voting_district_name_simple, "WARD ", "WARD 0"),
                                               voting_district_name_simple)) %>%
  select(-ward_num)


### PREPARE van_data

van_data$van_precinct_name <- toupper(van_data$van_precinct_name)
van_data$van_precinct_name <- str_remove(van_data$van_precinct_name, " -")

temp <- full_join(van_data, geo_data, by = c("van_precinct_name" = "voting_district_name_simple"))

temp <- temp %>%
  filter(!is.na(van_precinct_id) & !is.na(voting_dis))

write.csv(temp, "data/convert_van_ward.csv")








