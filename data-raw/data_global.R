## code to prepare `data_global` dataset goes here

library(dplyr)

tab_global <- readr::read_rds('inst/extdata/rds/latest_global_data.rds')

tab_global <- tab_global %>%
  mutate(
    countriesAndTerritories = recode(
      countriesAndTerritories,
      'Cases_on_an_international_conveyance_Japan' = 'Cruise_ship',
      'United_States_of_America' = 'USA',
      'United_Kingdom' = 'UK'
    )
  )

tab_global_total <- tab_global %>%
  mutate(popData2019 = as.numeric(popData2019)) %>%
  group_by(dateRep) %>%
  summarise(across(c("deaths", "cases", "popData2019"), sum, na.rm = TRUE)) %>%
  mutate(countriesAndTerritories = 'Global')

data_global <- bind_rows(tab_global, tab_global_total)

usethis::use_data(data_global, overwrite = TRUE)
