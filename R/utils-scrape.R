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
