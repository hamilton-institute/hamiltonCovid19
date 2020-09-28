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

  global_data <- hamiltonCovid19::global_data
  # tab_last_updated <- hamiltonCovid19::tab_last_updated

  interventions_data <- hamiltonCovid19::interventions_data

  callModule(
    mod_summary_ireland_server,
    "summary_ireland_ui_1",
    irish_data,
    irish_county_data
  )

  callModule(
    mod_summary_global_server,
    "summary_global_ui_1",
    global_data
  )

  callModule(
    mod_map_server,
    "map_ui_1",
    irish_county_data
  )

  callModule(
    mod_graphs_server,
    "graphs_ui_1",
    global_data
  )

  callModule(
    mod_animations_server,
    "animations_ui_1",
    global_data
  )

  callModule(
    mod_interventions_server,
    "interventions_ui_1",
    global_data,
    interventions_data
  )

}
