#' mod_summary_global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_global_ui <- function(id){
  ns <- NS(id)
  table_box_height <- "630px"
  tagList(
    fluidRow(
      col_12(
        tags$h2("Global"),
        tags$hr()
      )
    ),
    fluidRow(
      col_5(
        bs4Dash::bs4ValueBoxOutput(ns("wCasesBox"), width = 12),
        bs4Dash::bs4ValueBoxOutput(ns("wDeathsBox"), width = 12)
      ),
      col_3(
        bs4Dash::bs4ValueBoxOutput(ns("bigDailyBox"), width = 12),
        bs4Dash::bs4ValueBoxOutput(ns("worstHitCountryBox"), width = 12)
      ),
      col_4(
        bs4Dash::bs4ValueBoxOutput(ns("highest14DayGlobal"), width = 12),
        #bs4Dash::bs4ValueBoxOutput(ns("highest14DayEurope"), width = 12)
        bs4Dash::bs4ValueBoxOutput(ns("highestVaccination"), width = 12)
      )
    ),
    fluidRow(
      column(
        width = 3,
        offset = 9,
        shinyWidgets::pickerInput(
          inputId = ns("selVariable"),
          label = "Select variable",
          choices = get_summary_variables(),
          selected = 'totalVaccinations',
          multiple = FALSE,
          width = "98%"
        )
      )
    ),
    fluidRow(
      col_4(
        custom_box(
          width = 12,
          height = table_box_height,
          title = htmlOutput(ns("dailyDeathsTitle")),
          reactable::reactableOutput(ns("highestDaily"))
        )
      ),
      col_4(
        custom_box(
          width = 12,
          height = table_box_height,
          HTML(
            fa_icon(name = "exclamation-triangle", fill = "#d81b60"),
            "Total deaths"
          ),
          reactable::reactableOutput(ns("highestTotal"))
        )
      ),
      col_4(
        custom_box(
          width = 12,
          height = table_box_height,
          title = htmlOutput(ns("customTableTitle")),
          reactable::reactableOutput(ns("customTable"))
        )
      )
    )
  )
}

#' mod_summary_global Server Function
#'
#' @noRd
mod_summary_global_server <- function(input, output, session, global_data) {


  ns <- session$ns

  latest_global_data <- reactive({
    global_data %>%
      dplyr::filter(Date == max(Date))
  })

  output$wCasesBox <- bs4Dash::renderbs4ValueBox({
    global_data %>%
      dplyr::filter(countriesAndTerritories == "Global") %>%
      value_box_counts(
        variable = totalCases,
        title = "cases",
        icon = "globe",
        status = "success"
      )
  })

  output$wDeathsBox <- bs4Dash::renderbs4ValueBox({
    global_data %>%
      dplyr::filter(countriesAndTerritories == "Global") %>%
      value_box_counts(
        variable = totalDeaths,
        title = "deaths",
        icon = "cross",
        status = "success"
      )
  })

  output$bigDailyBox <- bs4Dash::renderbs4ValueBox({

    daily_death <- latest_global_data() %>%
      dplyr::filter(countriesAndTerritories != 'Global') %>%
      dplyr::slice_max(deaths, n = 1)

    value_box_countries(
      tab = daily_death,
      variable = deaths,
      title = "Most deaths today: ",
      icon = "arrow-up"
    )

  })

  output$worstHitCountryBox <- bs4Dash::renderbs4ValueBox({
    worst_countries <- global_data %>%
      dplyr::filter(countriesAndTerritories != 'Global') %>%
      dplyr::slice_max(deathsPerMillion, n = 1)
      #dplyr::slice_max(totalDeaths, n = 1)

    value_box_countries(
      tab = worst_countries,
      variable = totalDeaths,
      title = "Most deaths per million people: ",
      icon = "arrow-up"
    )

  })




  output$highest14DayGlobal <- bs4Dash::renderbs4ValueBox({

    highest14Day <- latest_global_data() %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        popData2019 > 1e6
      ) %>%
      dplyr::slice_max(last14per100k, n = 1)

    value_box_countries(
      tab = highest14Day,
      variable = last14per100k,
      title = "Highest 14-day cases per 100k: ",
      icon = "arrow-up"
    )

  })

  # output$highest14DayEurope <- bs4Dash::renderbs4ValueBox({
  #
  #   highest14Day <- latest_global_data() %>%
  #     dplyr::filter(
  #       countriesAndTerritories != 'Global',
  #       continentExp == "Europe & Central Asia",
  #       popData2019 > 1e6
  #     ) %>%
  #     dplyr::slice_max(last14per100k, n = 1)
  #
  #   value_box_countries(
  #     tab = highest14Day,
  #     variable = last14per100k,
  #     title = "Highest 14-day cases per 100k in Europe & Central Asia: ",
  #     icon = "arrow-up"
  #   )
  #
  # })

  output$highestVaccination <- bs4Dash::renderbs4ValueBox({

    highestVaxx <- latest_global_data() %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        popData2019 > 1e6
      ) %>%
      dplyr::slice_max(vaccinationDosesPer100k, n = 1)

    value_box_countries(
      tab = highestVaxx,
      variable = vaccinationDosesPer100k,
      title = "Highest vaccination doses per 100k: ",
      icon = "arrow-up"
    )

  })

  output$dailyDeathsTitle <- renderUI({
    HTML(
      fa_icon(name = "calendar-day", fill = "#3d9970"),
      paste0("Daily deaths: ", format(max(global_data$Date), '%d-%b-%Y'))
    )
  })

  output$highestDaily <- reactable::renderReactable({
    latest_global_data() %>%
      dplyr::filter(countriesAndTerritories != "Global") %>%
      dplyr::arrange(desc(deaths)) %>%
      dplyr::select(
        Country = countriesAndTerritories,
        Continent = continentExp,
        `Daily deaths` = deaths,
      ) %>%
      summaryTab_table()
  })

  output$highestTotal <- reactable::renderReactable({
    latest_global_data() %>%
      dplyr::filter(countriesAndTerritories != "Global") %>%
      dplyr::arrange(desc(totalDeaths)) %>%
      dplyr::select(
        Country = countriesAndTerritories,
        Continent = continentExp,
        `Total deaths` = totalDeaths
      ) %>%
      summaryTab_table()
  })

  output$customTableTitle <- renderUI({
    HTML(
      fa_icon(name = "chart-line", fill = "#3d9970"),
      get_variable_name(input$selVariable, get_summary_variables())
    )
  })

  output$customTable <- reactable::renderReactable({
    if (input$selVariable %in% c("cases", "totalCases","totalVaccinations","vaccinationDosesPer100k")) {
      latest_global_data() %>%
        dplyr::filter(countriesAndTerritories != "Global") %>%
        dplyr::arrange(desc(.data[[input$selVariable]])) %>%
        dplyr::select(
          Country = countriesAndTerritories,
          Continent = continentExp,
          Value = input$selVariable
        ) %>%
        summaryTab_table()
    } else {
      if (input$selVariable %in% c('changeDeaths', "changeCases")) {
        latest_global_data() %>%
          dplyr::filter(
            countriesAndTerritories != 'Global',
            .data[[input$selVariable]] != 0
          ) %>%
          dplyr::arrange(desc(.data[[input$selVariable]])) %>%
          dplyr::select(
            Country = countriesAndTerritories,
            Continent = continentExp,
            Value = input$selVariable
          ) %>%
          summaryTab_table()

      } else if (input$selVariable %in% c("last14per100k","last14deathsper100k")) {
        latest_global_data() %>%
          dplyr::filter(
            countriesAndTerritories != "Global",
            popData2019 > 1e6
          ) %>%
          dplyr::mutate(
            Value = .data[[input$selVariable]]
          ) %>%
          dplyr::arrange(desc(Value)) %>%
          dplyr::select(
            Country = countriesAndTerritories,
            Continent = continentExp,
            Value
          ) %>%
          dplyr::mutate(Value = format_decimal_number(Value, numeric = FALSE)) %>%
          summaryTab_table()
      }
    }
  })

}

