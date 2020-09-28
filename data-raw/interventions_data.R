## code to prepare `data_interventions` dataset goes here

library(dplyr)

interventions_data <- readxl::read_excel(
  'data-raw/xlsx/latest_interventions_data.xlsx',
  sheet = "Database"
)

interventions_data <- interventions_data %>%
  janitor::clean_names() %>%
  mutate(
    country = recode(
      country,
      'United States of America' = 'USA',
      'United Kingdom' = 'UK',
      "Czech Republic" = 'Czechia'
    ),
    measure = stringr::str_to_sentence(measure),
    measure = stringr::str_squish(measure)
  )

usethis::use_data(interventions_data, overwrite = TRUE)
