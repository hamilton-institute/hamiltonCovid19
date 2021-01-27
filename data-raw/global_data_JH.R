## Update global_data dataset
## On Dec 14, 2020, the ECDC data became weekly instead of daily. So
# we started to use the Jonhs Hopkins data, in the global_data_JH.R file.

usethis::ui_todo("Checking update for global_data dataset...")

`%>%` <- magrittr::`%>%`

remotes::install_github("joachim-gassen/tidycovid19", upgrade = "never", quiet = TRUE)

raw_global_data <- tidycovid19::download_merged_data(
  cached = TRUE,
  silent = TRUE
)

if (is.null(raw_global_data)) {
  message("Failed to download the data using the tidycovid19 package.")
} else {
  # Testing new dataset
  old_global_data <- readr::read_rds("data-raw/rds/raw_global_data_JH.rds")

  tab_duplicates <- raw_global_data %>%
    dplyr::count(date, country) %>%
    dplyr::filter(n > 1)

  needed_columns <- c(
    "date",
    "country",
    "confirmed",
    "deaths",
    "population",
    "region"
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
        "data-raw/rds/raw_global_data_JH.rds",
        compress = "xz"
      )

      global_data <- raw_global_data %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Date = as.Date(date, tryFormats = "%d/%m/%Y")) %>%
        dplyr::select(
          Date,
          countriesAndTerritories = country,
          totalCases = confirmed,
          totalDeaths = deaths,
          popData2019 = population,
          continentExp = region
        ) %>%
        dplyr::mutate(
          countriesAndTerritories = dplyr::case_when(
            countriesAndTerritories == 'United States' ~ 'USA',
            countriesAndTerritories == 'United Kingdom' ~ 'UK',
            TRUE ~ countriesAndTerritories
          )
        )

      global_data_totals <- global_data %>%
        dplyr::mutate(popData2019 = as.numeric(popData2019)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(
          dplyr::across(c("totalCases", "totalDeaths", "popData2019"), sum, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(countriesAndTerritories = 'Global')

      global_data <- dplyr::bind_rows(global_data, global_data_totals)

      global_data <- global_data %>%
        dplyr::group_by(countriesAndTerritories) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
          cases = totalCases - dplyr::lag(totalCases),
          deaths = totalDeaths - dplyr::lag(totalDeaths),
          changeCases = cases - dplyr::lag(cases),
          changeDeaths = deaths - dplyr::lag(deaths),
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
