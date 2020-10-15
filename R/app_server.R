#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Data

  data_repo <- "https://github.com/curso-r/hamiltonCovid19/raw/master/data-raw/rds/"

  global_data <- download_from_github(
    github_path = paste0(data_repo, "global_data.rds")
  )

  irish_data <- download_from_github(
    github_path = paste0(data_repo, "irish_data.rds")
  )

  irish_county_data <- hamiltonCovid19::irish_county_data

  # tab_last_updated <- hamiltonCovid19::tab_last_updated

  interventions_data <- hamiltonCovid19::interventions_data

  # Options

  options(reactable.theme = theme_reactable())

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
