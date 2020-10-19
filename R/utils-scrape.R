#' Scrape ECDC data
#'
#' Scrape data from the European Centre for Disease Control
#'
#' @return A data.frame.
#' @export
#'
scrape_ecdc_data <- function(url) {
  read.csv(url, na.strings = "", fileEncoding = "UTF-8-BOM")
}

#' Scrape Irish government data (country)
#'
#' Scrape irish data from Irish government
#'
#' @return A data.frame.
#' @export
#'
scrape_irl_data <- function(url) {
  url %>%
    RCurl::getURL() %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "properties")
}

#' Scrape Irish government data (counties)
#'
#' Scrape irish county data from Irish government
#'
#' @return A data.frame.
#' @export
#'
scrape_irl_county_data <- function(url) {
  read.csv(url, stringsAsFactors = FALSE)
}

download_from_github <- function(github_path) {
  file_name <- tempfile("temp.rds")
  download.file(
    url = github_path,
    destfile = file_name
  )
  readRDS(file_name)
}
