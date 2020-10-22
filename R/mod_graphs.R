#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        offset = 1,
        country_picker(default_countries(), id = ns("sel_ctry")),
      ),
      col_3(
        uiOutput(ns("ui_sel_var"))
      ),
      col_3(
        shinyWidgets::pickerInput(
          inputId = ns("sel_axis"),
          label = "Select horizontal axis",
          choices = c(
            'Date',
            'Days since 1st case',
            'Days since 10th case',
            'Days since 1st death',
            'Days since 10th death'
          ),
          selected = c('Date'),
          multiple = FALSE,
          width = "100%"
        )
      )
    ),
    fluidRow(
      col_10(
        offset = 1,
        bs4Dash::bs4Alert(
          width = 12,
          title = "Select only one country to select multiple variables.",
          status = "info"
        )
      )
    ),
    fluidRow(
      col_10(
        offset = 1,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("CountryPlot"),
            height = "500px"
          ),
          color = "#1E90FF"
        )
      )
    )
  )
}

#' graphs Server Function
#'
#' @noRd
mod_graphs_server <- function(input, output, session, global_data) {
  ns <- session$ns

  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "sel_ctry",
    choices = global_data %>%
      dplyr::pull(countriesAndTerritories) %>%
      unique(),
    selected = default_countries()
  )

  output$ui_sel_var <- renderUI({

    trigger_value <- runif(1)

    var_selected <- ifelse(
      isTruthy(isolate(input$sel_var)),
      remove_trigger_value(isolate(input$sel_var)),
      "totalCases14Days"
    )

    if (!isTruthy(input$sel_ctry)) {
      shinyWidgets::pickerInput(
        inputId = ns("sel_var"),
        label = "Select variables",
        choices = "",
        selected = "",
        multiple = FALSE,
        width = "100%"
      )
    } else if (length(input$sel_ctry) > 1) {
      shinyWidgets::pickerInput(
        inputId = ns("sel_var"),
        label = "Select variables",
        choices = create_trigger_value(get_graph_variables(), trigger_value),
        selected = create_trigger_value(var_selected, trigger_value),
        multiple = FALSE,
        width = "100%"
      )
    } else {
      shinyWidgets::pickerInput(
        inputId = ns("sel_var"),
        label = "Select variables",
        choices = create_trigger_value(get_graph_variables(), trigger_value),
        selected = create_trigger_value(var_selected, trigger_value),
        multiple = TRUE,
        width = "100%"
      )
    }

  })

  countries_tab <- reactive({

    validate(
      need(
        isTruthy(input$sel_var),
        "Please select some countries. Use Global for worldwide values.")
    )

    global_data %>%
      dplyr::filter(countriesAndTerritories %in% isolate(input$sel_ctry))

  })

  x_pick <- reactive({
    if (input$sel_axis == "Date") {
      x_pick <- c("Date" = "Date")
    } else {
      purrr::set_names("days_since", input$sel_axis)
    }
  })

  plot_tab <- reactive({

    tab <- countries_tab()
    variables <- remove_trigger_value(isolate(input$sel_var))
    x_var_name <- names(x_pick())

    if (x_var_name %in% c('Days since 1st case', 'Date')) {
      tab <- tab %>% dplyr::filter(totalCases > 0)
    } else if (x_var_name == 'Days since 1st death') {
      tab <- tab %>% dplyr::filter(totalDeaths > 0)
    } else if (x_var_name == 'Days since 10th death') {
      tab <- tab %>% dplyr::filter(totalDeaths >= 10)
    } else if (x_var_name == 'Days since 10th case') {
      tab <- tab %>% dplyr::filter(totalCases >= 10)
    }

    tab <- tab %>%
      dplyr::select(
        Date, countriesAndTerritories, popData2019,
        dplyr::one_of(variables)
      ) %>%
      dplyr::group_by(countriesAndTerritories) %>%
      dplyr::mutate(days_since = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        names_to = 'Type',
        values_to = 'Value',
        -c(Date, countriesAndTerritories, popData2019, days_since)
      )

    tab <- tab %>%
      dplyr::mutate(
        data_point = paste0(
          "\ncountry: ",
          .data[["countriesAndTerritories"]],
          "\nx_axis: ",
          .data[[x_pick()]],
          "\n",
          "y_axis: ",
          formatC(
            signif(Value, digits = 3),
            digits = 3,
            format = "fg",
            flag = "#"
          )
        ),
        Type = get_variable_name(
          Type,
          get_graph_variables(),
          remove_log_sqrt  = FALSE
        )
      )

  })

  output$CountryPlot <- plotly::renderPlotly({

    req(plot_tab())

    variables <- remove_trigger_value(isolate(input$sel_var))

    graphs_tab_plot(plot_tab(), variables, isolate(x_pick()))

  })

}
