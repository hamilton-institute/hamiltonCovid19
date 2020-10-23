## Update irish_county_data dataset

`%>%` <- magrittr::`%>%`

url_irl <- "http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv"

raw_irish_county_data <- tryCatch(
  read.csv(url_irl, stringsAsFactors = FALSE),
  error = function(x) NULL
)

if (is.null(raw_irish_county_data)) {
  message(paste0("Failed to connect to ", url_irl))
} else {
  old_irish_county_data <- readr::read_rds("data-raw/rds/raw_irish_county_data.rds")

  if (identical(old_irish_county_data, raw_irish_county_data) |
      nrow(old_irish_county_data) > nrow(raw_irish_county_data)) {
    message("Nothing to update in irish_county_data.")
  } else {
    readr::write_rds(
      raw_irish_county_data,
      "data-raw/rds/raw_irish_county_data.rds",
      compress = "xz"
    )

    geo_irish_county <- sf::st_read("data-raw/geojson/counties_simple.geojson")

    irish_county_data <- raw_irish_county_data %>%
      dplyr::mutate(Date = as.Date(TimeStamp)) %>%
      # dplyr::add_count(CountyName) %>%
      # dplyr::filter(n == max(n)) %>%
      dplyr::arrange(Date, CountyName) %>%
      dplyr::group_by(CountyName) %>%
      dplyr::mutate(DailyCases = c(0, diff(ConfirmedCovidCases))) %>%
      dplyr::mutate(
        last14per100k_raw = RcppRoll::roll_sum(
          DailyCases,
          14,
          align = "right",
          fill = 0
        ),
        last14per100k = 1e5 * last14per100k_raw / PopulationCensus16,
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        geo_irish_county,
        by = c("CountyName" = "NAME_TAG")
      ) %>%
      sf::st_as_sf() %>%
      dplyr::select(
        Date,
        CountyName,
        ConfirmedCovidCases:ConfirmedCovidRecovered,
        last14per100k,
        LATITUDE, LONGITUDE,
        geometry
      )

    deploy_app <- TRUE

    usethis::use_data(irish_county_data, overwrite = TRUE)
  }
}

