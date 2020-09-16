#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Data

  irish_data <- hamiltonCovid19::irish_data
  irish_county_data <- hamiltonCovid19::irish_county_data

  tab_last_updated <- hamiltonCovid19::tab_last_updated

  callModule(mod_summary_server, "summary_ui_1")
}
