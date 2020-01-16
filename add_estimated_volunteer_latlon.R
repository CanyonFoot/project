library(tidyverse)
imputed_van_loc <- read_csv("data/All_locations_with_imputed_van_precinct_id.csv") %>%
  select(latitude, longitude, imputed_precinct_id)
volunteers <- read_csv("data/vdim_volunteer_sample.csv")

volunteers_impute <- left_join(volunteers, imputed_van_loc, by = c("van_precinct_id" = "imputed_precinct_id")) %>%
  mutate(latitude = if_else(is.na(latitude.x), latitude.y, latitude.x),
         longitude = if_else(is.na(longitude.x), longitude.y, longitude.x)) %>%
  select(-c(latitude.x, latitude.y, longitude.y, longitude.x))

write.csv(volunteers_impute, "data/volunteers_imputed.csv")