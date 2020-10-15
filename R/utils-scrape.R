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


download_from_github <- function(github_path) {
  file_name <- tempfile("temp.rds")
  download.file(
    url = github_path,
    destfile = file_name
  )
  readRDS(file_name)
}
