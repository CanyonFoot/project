library(tidyverse)
geo_data <- read_csv("data/full_geo_ids")
survey <- read_csv("data/sample_vfact_contactcontact.csv")
respondents <- read_csv("data/voters.csv")
respondents <- respondents %>%
  select(person_id, van_precinct_id)

survey_filter <- survey %>%
  mutate(person_id = as.numeric(person_id)) %>%
  filter(unique_id_flag == TRUE & supportid != 0) %>%
  select(supportid, person_id)

  
survey_voters_geo <- inner_join(survey_filter, respondents) %>%
  inner_join(geo_data)

final_support_data <- survey_voters_geo %>%
  group_by(van_precinct_id) %>%
  summarise(total_count = n(), sup_count = sum(supportid == 1), sup_prop = mean(supportid == 1)) %>%
  full_join(geo_data)



write.csv(final_support_data, "data/geo_support.csv")








