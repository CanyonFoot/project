library(tidyverse)

voting_wards <- read_csv("data/convert_van_ward.csv")
turfs <- read_csv("data/NH_Turfs.csv")


wards_with_turf <- full_join(voting_wards, turfs) %>%
  select(-X1)

write.csv(wards_with_turf, "data/full_geo_ids")