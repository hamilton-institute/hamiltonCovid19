#' animations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_animations_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        offset = 1,
        uiOutput(ns("ui_sel_ctry"))
      ),
      col_3(
        shinyWidgets::pickerInput(
          inputId = ns("sel_horiz"),
          label = "Select horizontal axis",
          choices = get_anim_variables(),
          selected = 'sqrtTotalCases',
          multiple = FALSE
        )
      ),
      col_3(
        shinyWidgets::pickerInput(
          inputId = ns("sel_vert"),
          label = "Select vertical axis",
          choices = get_anim_variables(),
          selected = 'sqrtTotalDeaths',
          multiple = FALSE
        )
      )
    ),
    fluidRow(
      col_10(
        offset = 1,
        uiOutput(ns("ui_theDate"))
      )
    ),
    fluidRow(
      col_10(
        offset = 1,
          plotOutput(ns("AnimPlot"), height = "500px")
      )
    )
  )
}

#' animations Server Function
#'
#' @noRd
mod_animations_server <- function(input, output, session, global_data) {
  ns <- session$ns

  output$ui_sel_ctry <- renderUI({
    global_data %>%
      dplyr::pull(countriesAndTerritories) %>%
      unique() %>%
      country_picker(id = ns("sel_ctry"))
  })

  min_date <- reactive({

    req(input$sel_ctry)

    variables <- paste(input$sel_horiz, input$sel_vert)

    tab <- global_data %>%
      dplyr::filter(countriesAndTerritories %in% input$sel_ctry)

    if (stringr::str_detect(variables, "cases")) {
      tab <- tab %>%
        dplyr::filter(cases > 0)
    }

    if (stringr::str_detect(variables, "deaths")) {
      tab <- tab %>%
        dplyr::filter(deaths > 0)
    }

    min(tab$Date)

  })

  output$ui_theDate <- renderUI({

    req(min_date())

    sliderInput(
      ns("theDate"),
      "Date (click play or move slider)",
      min = min_date(),
      max = max(global_data$Date),
      value = max(global_data$Date) - months(1),
      width = "75%",
      timeFormat = "%d/%b",
      animate = animationOptions(interval = 1000, loop = FALSE)
    )

  })

  output$AnimPlot <- renderPlot({

    req(input$theDate)

    ani_graph(
      tab = global_data,
      ctry = input$sel_ctry,
      x_pick = input$sel_horiz,
      y_pick = input$sel_vert,
      date = input$theDate
    )

  })

}

