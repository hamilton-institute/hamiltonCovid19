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
      col_4(
        custom_box(
          width = 12,
          title = htmlOutput(ns("dailyDeathsTitle")),
          reactable::reactableOutput(ns("highestDaily"))
        )
      ),
      col_4(
        custom_box(
          width = 12,
          title = HTML(
            fa_icon(name = "exclamation-triangle", fill = "#d81b60"),
            "Total deaths"
          ),
          reactable::reactableOutput(ns("highestTotal"))
        )
      ),
      col_4(
        custom_box(
          width = 12,
          title = HTML(
            fa_icon(name = "chart-line", fill = "#3c8dbc"),
            "Deaths increase from yesterday"
          ),
          reactable::reactableOutput(ns("biggestChange"))
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
        `Total deaths` = totalDeaths
      ) %>%
      summaryTab_table()
  })

  output$biggestChange <- reactable::renderReactable({
    latest_global_data() %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        deaths != 0
      ) %>%
      dplyr::select(
        Country = countriesAndTerritories,
        `Change in deaths` = changeDeaths
      ) %>%
      dplyr::arrange(desc(`Change in deaths`)) %>%
      summaryTab_table()
  })

}

