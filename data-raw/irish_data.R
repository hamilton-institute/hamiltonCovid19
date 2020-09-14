## code to prepare `latest_irish_data` dataset goes here

irish_data <-  readRDS('data-raw/rds/latest_irish_data.rds') %>%
  dplyr::as_tibble() %>%
  dplyr::filter(dplyr::across(.fns = ~ !is.na(.x))) %>%
  dplyr::select(
    Date,
    TotalConfirmedCovidCases,
    TotalCovidDeaths,
    HospitalisedCovidCases,
    RequiringICUCovidCases
  )

usethis::use_data(irish_data, overwrite = TRUE)
