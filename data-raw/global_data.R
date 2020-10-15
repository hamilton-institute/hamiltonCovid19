## Update global_data dataset

devtools::load_all()

url_ecdc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
raw_global_data <- purrr::possibly(scrape_ecdc_data, otherwise = NULL)(url_ecdc)

if (is.null(raw_global_data)) {
  stop(paste0("Failed to download a CSV file from ", url_ecdc))
}

old_global_data <- readr::read_rds("data-raw/rds/raw_global_data.rds")

if (identical(raw_global_data, old_global_data)) {
  stop("Nothing to update.")
}

readr::write_rds(
  raw_global_data,
  "data-raw/rds/raw_global_data.rds",
  compress = "xz"
)

# Fixing date
new_global_data <- raw_global_data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Date = as.Date(dateRep, tryFormats = "%d/%m/%Y")) %>%
  dplyr::select(-dateRep, -day, -month, -year)

# Fixing country names
new_global_data <- new_global_data %>%
  dplyr::mutate(
    countriesAndTerritories = dplyr::case_when(
      countriesAndTerritories == 'Cases_on_an_international_conveyance_Japan' ~
        'Cruise_ship',
      countriesAndTerritories == 'United_States_of_America' ~ 'USA',
      countriesAndTerritories == 'United_Kingdom' ~ 'UK',
      TRUE ~ countriesAndTerritories
    ),
    countriesAndTerritories = stringr::str_replace_all(
      string = countriesAndTerritories,
      pattern = "_",
      replacement = " "
    )
  )

# Calculating totals
global_data_totals <- new_global_data %>%
  dplyr::mutate(popData2019 = as.numeric(popData2019)) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(
    dplyr::across(c("deaths", "cases", "popData2019"), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    countriesAndTerritories = 'Global'
  )

new_global_data <- dplyr::bind_rows(new_global_data, global_data_totals)

# Log e cumulative values
new_global_data <- new_global_data %>%
  dplyr::group_by(countriesAndTerritories) %>%
  dplyr::arrange(Date, .by_group = TRUE) %>%
  dplyr::mutate(
    changeCases = cases - lag(cases),
    changeDeaths = deaths - lag(deaths),
    totalCases = cumsum(cases),
    totalDeaths = cumsum(deaths),
    logTotalCases = log(totalCases),
    logTotalDeaths = log(totalDeaths),
    casesPerMillion = 1e6 * totalCases / popData2019,
    deathsPerMillion = 1e6 * totalDeaths / popData2019
  ) %>%
  dplyr::rename(
    totalCases14Days = Cumulative_number_for_14_days_of_COVID.19_cases_per_100000
  ) %>%
  dplyr::ungroup()

# Saving dataset
readr::write_rds(new_global_data, "data-raw/rds/global_data.rds", compress = "xz")
