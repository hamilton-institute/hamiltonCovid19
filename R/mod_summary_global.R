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
  table_box_height <- "540px"
  tagList(
    fluidRow(
      col_5(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("wCasesBox"), width = 12),
          color="#1E90FF"
        ),
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("wDeathsBox"), width = 12),
          color="#1E90FF"
        )
      ),
      col_3(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("bigDailyBox"), width = 12),
          color="#1E90FF"
        ),
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("worstHitCountryBox"), width = 12),
          color="#1E90FF"
        )
      ),
      col_4(
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("increaseDeathBox"), width = 12),
          color="#1E90FF"
        ),
        shinycssloaders::withSpinner(
          bs4Dash::bs4ValueBoxOutput(ns("bigDecreaseBox"), width = 12),
          color="#1E90FF"
        )
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
          selected = 'totalCases14Days',
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
          div(
            style = "overflow-y: auto; height: 500px;",
            reactable::reactableOutput(ns("highestDaily"))
          )
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
          div(
            style = "overflow-y: auto; height: 500px;",
            reactable::reactableOutput(ns("highestTotal"))
          )
        )
      ),
      col_4(
        custom_box(
          width = 12,
          height = table_box_height,
          title = htmlOutput(ns("customTableTitle")),
          div(
            style = "overflow-y: auto; height: 500px;",
            reactable::reactableOutput(ns("customTable"))
          )
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
        title = "Global: Diagnoses",
        icon = "globe",
        status = "success"
      )
  })

  output$wDeathsBox <- bs4Dash::renderbs4ValueBox({
    global_data %>%
      dplyr::filter(countriesAndTerritories == "Global") %>%
      value_box_counts(
        variable = totalDeaths,
        title = "Global: Deaths",
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
      dplyr::slice_max(totalDeaths, n = 1)

    value_box_countries(
      tab = worst_countries,
      variable = totalDeaths,
      title = "Most deaths overall: ",
      icon = "arrow-up"
    )

  })

  output$increaseDeathBox <- bs4Dash::renderbs4ValueBox({

    biggest_increase <- latest_global_data() %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        deaths != 0
      ) %>%
      dplyr::slice_max(changeDeaths, n = 1)

    value_box_countries(
      tab = biggest_increase,
      variable = changeDeaths,
      title = "Biggest increase in deaths since</br> previous day: ",
      icon = "arrow-up"
    )

  })

  output$bigDecreaseBox <- bs4Dash::renderbs4ValueBox({

    biggest_decrease <- latest_global_data() %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        deaths != 0
      ) %>%
      dplyr::slice_min(changeDeaths, n = 1)

    value_box_countries(
      tab = biggest_decrease,
      variable = changeDeaths,
      title = "Biggest reduction in deaths since</br> previous day: ",
      icon = "arrow-down"
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
    if (input$selVariable %in% c("cases", "totalCases")) {
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

      } else if (input$selVariable == "totalCases14Days") {
        latest_global_data() %>%
          dplyr::filter(countriesAndTerritories != "Global") %>%
          dplyr::mutate(
            Value = round(.data[[input$selVariable]], 1)
          ) %>%
          dplyr::arrange(desc(Value)) %>%
          dplyr::select(
            Country = countriesAndTerritories,
            Continent = continentExp,
            Value
          ) %>%
          summaryTab_table()
      }
    }
  })

}

