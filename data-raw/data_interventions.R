## code to prepare `data_interventions` dataset goes here

library(dplyr)

data_interventions <- readxl::read_excel(
  'inst/extdata/xlsx/latest_interventions_data.xlsx',
  sheet = "Database"
)

data_interventions <- data_interventions %>%
  janitor::clean_names() %>%
  mutate(
    country = recode(
      country,
      'United States of America' = 'USA',
      'United Kingdom' = 'UK',
      "Czech Republic" = 'Czechia'
    )
  )

usethis::use_data(data_interventions, overwrite = TRUE)
