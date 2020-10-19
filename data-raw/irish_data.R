## Update irish_data dataset

`%>%` <- magrittr::`%>%`

url_irl <- "https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.geojson"

raw_irish_data <- tryCatch(
  url_irl %>%
    RCurl::getURL() %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "properties"),
  error = function(x) NULL
)

if (is.null(raw_irish_data)) {
  stop(paste0("Failed to connect to ", url_irl))
}

old_irish_data <- readr::read_rds("data-raw/rds/raw_irish_data.rds")

if (identical(old_irish_data, raw_irish_data)) {
  deploy_app <- TRUE
  stop("Nothing to update.")
}

readr::write_rds(
  raw_irish_data,
  "data-raw/rds/raw_irish_data.rds",
  compress = "xz"
)

irish_data <- raw_irish_data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  dplyr::filter(dplyr::across(.fns = ~ !is.na(.x))) %>%
  dplyr::select(
    Date,
    TotalConfirmedCovidCases,
    TotalCovidDeaths,
    HospitalisedCovidCases,
    RequiringICUCovidCases
  )

deploy_app <- TRUE

usethis::use_data(irish_data, overwrite = TRUE)
