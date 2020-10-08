## code to prepare `data_global` dataset goes here

library(dplyr)

global_data_raw <- readr::read_rds('data-raw/rds/latest_global_data.rds') %>%
  tibble::as_tibble()

global_data_raw <- global_data_raw %>%
  rename(Date = dateRep) %>%
  mutate(
    countriesAndTerritories = case_when(
      countriesAndTerritories == 'Cases_on_an_international_conveyance_Japan' ~
        'Cruise_ship',
      countriesAndTerritories == 'United_States_of_America' ~'USA',
      countriesAndTerritories == 'United_Kingdom' ~ 'UK',
      TRUE ~ countriesAndTerritories
    ),
    countriesAndTerritories = stringr::str_replace_all(countriesAndTerritories, "_", " ")
  )

global_data_totals <- global_data_raw %>%
  mutate(popData2019 = as.numeric(popData2019)) %>%
  group_by(Date) %>%
  summarise(
    across(c("deaths", "cases", "popData2019"), sum, na.rm = TRUE),
    across(c("day", "month", "year"), first)
  ) %>%
  mutate(
    countriesAndTerritories = 'Global'
  )

global_data <- bind_rows(global_data_raw, global_data_totals)

global_data <- global_data %>%
  group_by(countriesAndTerritories) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    changeCases = cases - lag(cases),
    changeDeaths = deaths - lag(deaths),
    totalCases = cumsum(cases),
    totalDeaths = cumsum(deaths),
    logTotalCases = log(totalCases),
    logTotalDeaths = log(totalDeaths),
    casesPerMillion = 1e6 * totalCases / popData2019,
    deathsPerMillion = 1e6 * totalDeaths / popData2019
  ) %>%
  rename(
    totalCases14Days = Cumulative_number_for_14_days_of_COVID.19_cases_per_100000
  ) %>%
  ungroup()

usethis::use_data(global_data, overwrite = TRUE)
