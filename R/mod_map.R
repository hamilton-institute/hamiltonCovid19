#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        custom_box(
          title = '14-day total cases per 100k residents by county',
          width = 12,
          height = 530,
          htmlOutput(ns("updatedText")),
          reactable::reactableOutput(
            ns("countyCasesTable"),
            height = "480px"
          ) %>%
            shinycssloaders::withSpinner(color = "#1E90FF")
        )
      ),
      col_8(
        custom_box(
          width = 12,
          height = 530,
          leaflet::leafletOutput(ns('covidMap'), height = 500) %>%
            shinycssloaders::withSpinner(color="#1E90FF")
        )
      )
    )
  )
}

#' map Server Function
#'
#' @noRd
mod_map_server <- function(input, output, session, irish_county_data){

  ns <- session$ns

  latest_irish_county_data <- reactive({
    irish_county_data %>%
      dplyr::filter(Date == max(Date))
  })

  output$updatedText <- renderUI({
    latest_date <- max(irish_county_data$Date)
    text <- paste0("Updated: ", latest_date)
    color <- paste0("color: ", status_para_cor("primary"), ";")
    tags$span(text, style = color)
  })

  output$countyCasesTable <- reactable::renderReactable({
    latest_irish_county_data() %>%
      dplyr::arrange(dplyr::desc(last14per100k)) %>%
      dplyr::select(CountyName, Value = last14per100k) %>%
      sf::st_drop_geometry() %>%
      reactable::reactable(
        defaultPageSize = 20,
        height = 480,
        searchable = FALSE,
        pagination = FALSE,
        rownames = FALSE
      )
  })

  output$covidMap <- leaflet::renderLeaflet({
    latest_irish_county_data() %>%
      ireland_map() %>%
      leaflet::addMarkers(
        lat = ~LATITUDE,
        lng = ~LONGITUDE,
        icon = trend_icon(),
        popup = leafpop::popupGraph(make_leaflet_popup_plots(irish_county_data))
      ) %>%
      leaflet::addLegend(
        pal = leaflet_map_pal(latest_irish_county_data()),
        title = 'Cases',
        values = ~log2(ConfirmedCovidCases),
        opacity = 1.0,
        labFormat = leaflet::labelFormat(transform = function(x) round(2^x))
      )
  })

}

