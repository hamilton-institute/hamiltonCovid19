## code to prepare `irish_county_data` dataset goes here

geo_irish_county <- sf::st_read("data-raw/geojson/counties_simple.geojson")

irish_county_data <- readr::read_rds(
  'data-raw/rds/latest_irish_county_data.rds'
) %>%
  dplyr::left_join(
    geo_irish_county,
    by = c("CountyName" = "NAME_TAG")
  ) %>%
  sf::st_as_sf()

usethis::use_data(irish_county_data, overwrite = TRUE)
