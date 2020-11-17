## Update global_data dataset

usethis::ui_todo("Checking update for global_data dataset...")

`%>%` <- magrittr::`%>%`

url_ecdc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

raw_global_data <- tryCatch(
  read.csv(url_ecdc, na.strings = "", fileEncoding = "UTF-8-BOM"),
  error = function(x) NULL
)

if (is.null(raw_global_data)) {
  message(paste0("Failed to download a CSV file from ", url_ecdc))
} else {
  # Testing new dataset
  old_global_data <- readr::read_rds("data-raw/rds/raw_global_data.rds")

  tab_duplicates <- raw_global_data %>%
    dplyr::count(dateRep, countriesAndTerritories) %>%
    dplyr::filter(n > 1)

  needed_columns <- c(
    "dateRep",
    "countriesAndTerritories",
    "cases",
    "deaths",
    "geoId",
    "countryterritoryCode",
    "popData2019",
    "continentExp"
  )

  missing_columns <- needed_columns[
    !needed_columns %in% names(raw_global_data)
  ]

  if (identical(raw_global_data, old_global_data)) {
    usethis::ui_done("Nothing to update in global_data.")
  } else if (nrow(tab_duplicates) > 0) {
    usethis::ui_oops("New data has duplicated Date/countriesAndTerritories entries. Dataset not updated.")
  } else if (length(missing_columns) > 0) {
    usethis::ui_oops(paste0(
      "The following columns are missing in the new dataset: ",
      paste(missing_columns, collapse = ", "),
      ". Dataset not updated."
    ))
  } else {

    # Updating dataset
    suppressMessages({
      readr::write_rds(
        raw_global_data,
        "data-raw/rds/raw_global_data.rds",
        compress = "xz"
      )

      global_data <- raw_global_data %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Date = as.Date(dateRep, tryFormats = "%d/%m/%Y")) %>%
        dplyr::select(
          Date,
          countriesAndTerritories,
          cases,
          deaths,
          geoId,
          countryterritoryCode,
          popData2019,
          continentExp
        )

      global_data <- global_data %>%
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

      global_data_totals <- global_data %>%
        dplyr::mutate(popData2019 = as.numeric(popData2019)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(
          dplyr::across(c("deaths", "cases", "popData2019"), sum, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(countriesAndTerritories = 'Global')

      global_data <- dplyr::bind_rows(global_data, global_data_totals)

      global_data <- global_data %>%
        dplyr::group_by(countriesAndTerritories) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
          changeCases = cases - lag(cases),
          changeDeaths = deaths - lag(deaths),
          totalCases = cumsum(cases),
          totalDeaths = cumsum(deaths),
          logp1Cases = suppressWarnings(log(cases + 1)),
          logp1Deaths = suppressWarnings(log(deaths + 1)),
          logp1TotalCases = log(totalCases + 1),
          logp1TotalDeaths = log(totalDeaths + 1),
          casesPerMillion = 1e6 * totalCases / popData2019,
          deathsPerMillion = 1e6 * totalDeaths / popData2019,
          last14per100k = RcppRoll::roll_sum(
            cases,
            14,
            align = "right",
            fill = 0
          ),
          last14per100k = 1e5 * last14per100k / popData2019
        ) %>%
        dplyr::ungroup()

      deploy_app <- TRUE

      # Saving dataset
      usethis::use_data(global_data, overwrite = TRUE)
    })
    usethis::ui_done("global_data dataset updated!")
  }
}
