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
      column(
        width = 12,
        shinyWidgets::pickerInput(
          inputId = ns("sel_data"),
          label = "Select data",
          choices = c(
            'Daily deaths' = "deaths",
            'Daily cases' = "cases",
            "Total deaths" = "totalDeaths",
            "Total cases" = "totalCases",
            "Deaths increased since yesterday" =  "changeDeaths",
            "Cases increased since yesterday" = "changeCases",
            "14-days per 100k residents" = "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000"
          ),
          selected = 'Daily deaths',
          multiple = FALSE
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        custom_box(
          width = 12,
          title = htmlOutput(ns("tableTitle")),
          reactable::reactableOutput(ns("highestDaily"))
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

  data_name <- reactive({
    switch(
      input$sel_data,
      "deaths" = 'Daily deaths',
      "cases" = 'Daily cases',
      "totalDeaths" = "Total deaths",
      "totalCases" = "Total cases",
      "changeDeaths" =  "Deaths increased since yesterday",
      "changeCases" = "Cases increased since yesterday" ,
      "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000" = "14-days per 100k residents"
    )
  })

  output$tableTitle <- renderUI({

      HTML(fa_icon(name = "calendar-day", fill = "#3d9970"),
           paste0(data_name(),": ", format(max(global_data$Date), '%d-%b-%Y')))
  })

  output$highestDaily <- reactable::renderReactable({
    data <- input$sel_data
    data_name <- data_name()

    if(input$sel_data %in% c("deaths", "cases", 'totalDeaths', "totalCases")) {
      latest_global_data() %>%
        dplyr::filter(countriesAndTerritories != "Global") %>%
        dplyr::arrange(desc(.data[[data]])) %>%
        dplyr::select(
          Country = countriesAndTerritories,
          Continent = continentExp,
          {{data_name}} := .data[[data]]
          ) %>%
        summaryTab_table()
    } else{
      if(input$sel_data %in% c('changeDeaths', "changeCases")) {
        var <- stringr::str_remove(input$sel_data, "change") %>% stringr::str_to_lower()
        name <- paste0("Change in ", var)
        latest_global_data() %>%
          dplyr::filter(countriesAndTerritories != 'Global',
                        .data[[var]] != 0) %>%
          dplyr::select(Country = countriesAndTerritories,
                        Continent = continentExp,
                        {{name}} := .data[[data]]) %>%
          dplyr::arrange(desc(.data[[name]])) %>%
          summaryTab_table()
      } else{
        latest_global_data() %>%
          dplyr::filter(countriesAndTerritories != "Global") %>%
          dplyr::mutate(
            Value = round(Cumulative_number_for_14_days_of_COVID.19_cases_per_100000,1)
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

