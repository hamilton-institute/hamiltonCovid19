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
      col_3(
        bs4Dash::box(
          title = 'Cases by County',
          width = 12,
          DT::dataTableOutput(ns("countyCasesTable")) %>%
            shinycssloaders::withSpinner(color = "#1E90FF")
        )
      ),
      col_9(
        bs4Dash::box(
          title = "COVID-19 in Ireland",
          width = 12,
          leaflet::leafletOutput(ns('covidMap')) %>%
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

  output$countyCasesTable <- DT::renderDataTable({

    latest_date <- max(irish_county_data$Date)

    latest_irish_county_data() %>%
      dplyr::arrange(dplyr::desc(ConfirmedCovidCases)) %>%
      dplyr::select(CountyName, `Number of Cases` = ConfirmedCovidCases) %>%
      sf::st_drop_geometry() %>%
      DT::datatable(
        caption = paste0("Updated: ", latest_date),
        options = list(
          pageLength = 20,
          scrollY = 'calc((100vh - 290px)/1.0)',
          searching = FALSE,
          paging = FALSE
        ),
        rownames = FALSE
      ) %>%
      DT::formatStyle(1, color = "#c8c8c8", target = "row")
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

