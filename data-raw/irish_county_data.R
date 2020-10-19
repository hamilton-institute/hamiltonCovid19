## Update irish_county_data dataset

devtools::load_all()

url_irl <- "http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv"

raw_irish_county_data <- purrr::possibly(scrape_irl_county_data, otherwise = NULL)(url_irl)

if (is.null(raw_irish_county_data)) {
  stop(paste0("Failed to connect to ", url_irl))
}

old_irish_county_data <- readr::read_rds("data-raw/rds/raw_irish_county_data.rds")

if (identical(old_irish_county_data, raw_irish_county_data)) {
  stop("Nothing to update.")
}

readr::write_rds(
  raw_irish_county_data,
  "data-raw/rds/raw_irish_county_data.rds",
  compress = "xz"
)

geo_irish_county <- sf::st_read("data-raw/geojson/counties_simple.geojson")

new_irish_county_data <- raw_irish_county_data %>%
  dplyr::mutate(Date = as.Date(TimeStamp)) %>%
  # dplyr::add_count(CountyName) %>%
  # dplyr::filter(n == max(n)) %>%
  dplyr::left_join(
    geo_irish_county,
    by = c("CountyName" = "NAME_TAG")
  ) %>%
  sf::st_as_sf() %>%
  dplyr::select(
    Date,
    CountyName,
    Lat, Long,
    ConfirmedCovidCases:ConfirmedCovidRecovered,
    geometry
  )

readr::write_rds(new_irish_data, "data-raw/rds/irish_data.rds", compress = "xz")

