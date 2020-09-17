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
    )
  )
}

#' mod_summary_global Server Function
#'
#' @noRd
mod_summary_global_server <- function(input, output, session, global_data) {
  ns <- session$ns

  output$wCasesBox <- bs4Dash::renderbs4ValueBox({
    global_data %>%
      dplyr::filter(countriesAndTerritories == "Global") %>%
      value_box_counts(
        variable = totalCases,
        title = "Global: Diagnoses",
        icon = "globe",
        status = "danger"
      )
  })

  output$wDeathsBox <- bs4Dash::renderbs4ValueBox({
    global_data %>%
      dplyr::filter(countriesAndTerritories == "Global") %>%
      value_box_counts(
        variable = totalDeaths,
        title = "Global: Deaths",
        icon = "cross",
        status = "danger"
      )
  })

  output$bigDailyBox <- bs4Dash::renderbs4ValueBox({

    daily_death <- global_data %>%
      dplyr::filter(countriesAndTerritories != 'Global', Date == max(Date)) %>%
      dplyr::slice_max(deaths, 1)

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
      dplyr::slice_max(totalDeaths, 1)

    value_box_countries(
      tab = worst_countries,
      variable = totalDeaths,
      title = "Most deaths overall: ",
      icon = "arrow-up"
    )

  })

  output$increaseDeathBox <- bs4Dash::renderbs4ValueBox({

    biggest_increase <- global_data %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        deaths != 0,
        Date == max(Date)
      ) %>%
      dplyr::slice_max(changeDeaths, 1)

    value_box_countries(
      tab = biggest_increase,
      variable = changeDeaths,
      title = "Biggest increase in deaths since</br> previous day: ",
      icon = "arrow-up"
    )

  })

  output$bigDecreaseBox <- bs4Dash::renderbs4ValueBox({

    biggest_decrease <- global_data %>%
      dplyr::filter(
        countriesAndTerritories != 'Global',
        deaths != 0,
        Date == max(Date)
      ) %>%
      dplyr::slice_min(changeDeaths, 1)

    value_box_countries(
      tab = biggest_decrease,
      variable = changeDeaths,
      title = "Biggest reduction in deaths since</br> previous day: ",
      icon = "arrow-up"
    )

  })


}

