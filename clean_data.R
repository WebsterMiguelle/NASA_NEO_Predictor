library(tidyverse)
library(janitor)

raw_data <- read_csv("asteroids_data.csv")

clean_asteroids <- raw_data |>
  clean_names() |>
  select(-id, -neo_id, -name, -short_name, -designation, -orbit_id,
         -nasa_url, -api_url, -close_approach_data, -sentry_data,
         -orbit_determination_date, -first_observation_date, 
         -last_observation_date, -epoch, -equinox, -perihelion_time,
         -orbit_class_desc, -orbit_class_range) |>
  mutate(potentially_hazardous = as.factor(potentially_hazardous),
          orbit_class_type = as.factor(orbit_class_type)) |>
  drop_na()

glimpse(clean_asteroids)

saveRDS(clean_asteroids,"clean_asteroids.rds")
