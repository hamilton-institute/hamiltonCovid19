#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ireland_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        tags$h2("Ireland"),
        tags$hr()
      )
    ),
    fluidRow(
      col_8(
        fluidRow(
          col_6(
            shinycssloaders::withSpinner(
              bs4Dash::bs4ValueBoxOutput(ns("ireCasesBox"), width = 12),
              color="#1E90FF"
            )
          ),
          col_6(
            shinycssloaders::withSpinner(
              bs4Dash::bs4ValueBoxOutput(ns("ireDeathsBox"), width = 12),
              color="#1E90FF"
            )
          )
        ),
        fluidRow(
          col_6(
            shinycssloaders::withSpinner(
              bs4Dash::bs4ValueBoxOutput(ns("ireHospBox"), width = 12),
              color="#1E90FF"
            )
          ),
          col_6(
            shinycssloaders::withSpinner(
              bs4Dash::bs4ValueBoxOutput(ns("ireICUBox"), width = 12),
              color="#1E90FF"
            )
          )
        )
      ),
      col_4(
        leaflet::leafletOutput(ns("irelandCovidMap"), height = 350)
      )
    ),
    fluidRow(
      col_12(
        tags$h2("Global"),
        tags$hr()
      )
    )
  )

}

#' summary Server Function
#'
#' @noRd
mod_summary_ireland_server <- function(input, output, session, irish_data,
                                       irish_county_data) {

  ns <- session$ns

  output$ireCasesBox <- bs4Dash::renderbs4ValueBox({
    value_box_counts(
      tab = irish_data,
      variable = TotalConfirmedCovidCases,
      title = "Ireland: Diagnoses",
      icon = "thermometer-three-quarters"
    )
  })

  output$ireDeathsBox <- bs4Dash::renderbs4ValueBox({
    value_box_counts(
      tab = irish_data,
      variable = TotalCovidDeaths,
      title = "Ireland: Deaths",
      icon = "exclamation-triangle"
    )
  })

  output$ireHospBox <- bs4Dash::renderbs4ValueBox({
    value_box_counts(
      tab = irish_data,
      variable = HospitalisedCovidCases,
      title = "Ireland: Hospitalised",
      icon = "hospital"
    )
  })

  output$ireICUBox <- bs4Dash::renderbs4ValueBox({
    value_box_counts(
      tab = irish_data,
      variable = RequiringICUCovidCases,
      title = "Ireland: ICU",
      icon = "briefcase-medical"
    )
  })

  output$irelandCovidMap <- leaflet::renderLeaflet({
    irish_county_data %>%
      dplyr::filter(Date == max(Date)) %>%
      ireland_map(title = "14-day cases per 100,000 residents")
  })

}

