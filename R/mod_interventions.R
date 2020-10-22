#' interventions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_interventions_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      col_4(
        uiOutput(ns("ui_sel_ctry"))
      ),
      col_4(
        shinyWidgets::pickerInput(
          inputId = ns("sel_var"),
          label = "Select a variable",
          choices = get_inter_variables(),
          selected = "deaths",
          multiple = FALSE
        )
      ),
      col_4(
        shinyWidgets::pickerInput(
          inputId = ns("sel_axis"),
          label = "Select a horizontal axis",
          choices = c(
            "Date" = "Date",
            "Days since measure introduced" = "days_since"
          ),
          selected = "days_since",
          multiple = FALSE
        )
      )
    ),
    fluidRow(
      col_4(
        uiOutput(ns("ui_sel_measure"))
      ),
      col_4(
        checkboxInput(
          inputId = ns("sel_smooth"),
          label = "Include smooth",
          value = FALSE,
        )
      ),
      col_4(
        checkboxInput(
          inputId = ns("sel_window"),
          label = "Include 2-week window\n (mouse-over for detail)",
          value = TRUE,
        )
      )
    ),
    fluidRow(
      col_12(
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("InterventionsPlot"), height = "500px"),
          color = "#1E90FF"
        )
      )
    )
  )

}

#' interventions Server Function
#'
#' @noRd
mod_interventions_server <- function(input, output, session, global_data,
                                     interventions_data) {

  ns <- session$ns

  output$ui_sel_ctry <- renderUI({
    shinyWidgets::pickerInput(
      inputId = ns("sel_ctry"),
      label = "Select countries",
      choices = unique(global_data$countriesAndTerritories),
      selected = "Ireland",
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })

  output$ui_sel_measure <- renderUI({

    measures <- interventions_data %>%
      dplyr::pull(measure) %>%
      unique() %>%
      sort()

    shinyWidgets::pickerInput(
      inputId = ns("sel_measure"),
      label = "Select intervention measure",
      choices = measures,
      selected = "Schools closure",
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })


  output$InterventionsPlot <- plotly::renderPlotly({

    req(input$sel_ctry, input$sel_measure)

    tab <- global_data %>%
      dplyr::filter(countriesAndTerritories %in% input$sel_ctry) %>%
      dplyr::select(
        Date,
        cases,
        deaths,
        totalCases,
        totalDeaths,
        logp1TotalCases,
        logp1TotalDeaths,
        casesPerMillion,
        deathsPerMillion,
        countriesAndTerritories,
        popData2019
      )

    validate(
      need(
        nrow(tab) > 0,
        "Please select some countries. Use Global for worldwide values."
      )
    )

    pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))

    country_colours <- tab %>%
      dplyr::arrange(dplyr::desc(popData2019)) %>%
      dplyr::distinct(countriesAndTerritories) %>%
      dplyr::mutate(
        Colours = pal(dplyr::n())
      ) %>%
      tibble::deframe()

    use_interventions <- interventions_data %>%
      dplyr::mutate(date_implemented = as.Date(date_implemented)) %>%
      dplyr::filter(
        country %in% input$sel_ctry,
        measure %in% input$sel_measure
      ) %>%
      dplyr::select(country, date_implemented, measure, comments) %>%
      dplyr::arrange(date_implemented, measure) %>%
      dplyr::distinct(date_implemented, country, measure, .keep_all = TRUE) %>%
      dplyr::group_by(date_implemented, country) %>%
      dplyr::summarise(
        measure2 = paste0(country, ": ", unique(measure), collapse = '; '),
        comments2 = paste0(country, ": ", unique(comments), collapse = '; '),
        Date = max(date_implemented),
        countriesAndTerritories = dplyr::first(country)
      ) %>%
      dplyr::ungroup() %>%
      na.omit() %>%
      dplyr::mutate(
        days_since = as.integer(
          as.Date(date_implemented) - as.Date(min(date_implemented))
        ),
        date_end = Date + lubridate::days(14),
        days_end = days_since + 14,
        data_point = measure2,
        Number = 0
      ) %>%
      dplyr::filter(days_since >= 0)

    validate(
      need(
        nrow(use_interventions) > 0,
        "Measure not found for this country"
      )
    )

    use_interventions2 <- use_interventions %>%
      dplyr::mutate(data_point = stringr::str_wrap(comments2))

    x_pick <- ifelse(input$sel_axis == "Date", "Date", "days_since")
    x_pick2 <- ifelse(input$sel_axis == "Date", "date_end", "days_end")

    tab <- tab %>%
      dplyr::group_by(countriesAndTerritories) %>%
      dplyr::mutate(
        days_since = as.integer(
          as.Date(Date) - as.Date(min(use_interventions$date_implemented)))
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        names_to = 'Type',
        values_to = 'Number',
        -c(Date, countriesAndTerritories, popData2019, days_since)
      )

    if(x_pick == 'days_since') {
      tab <- dplyr::filter(tab, days_since >= 0)
    }

    tab <- tab %>%
      dplyr::filter(Type %in% input$sel_var) %>%
      dplyr::mutate(
        data_point = paste0(
          "\ncountry: ",
          countriesAndTerritories,
          "\nx_axis: ",
          .data[[x_pick]],
          "\n",
          "y_axis: ",
          formatC(
            signif(Number, digits = 3),
            digits = 3, format = "fg",
            flag = "#"
          )
        )
      )

    n_countries <- dplyr::n_distinct(tab$countriesAndTerritories)
    n_vars <- length(input$sel_var)

    p <- tab %>%
      ggplot2::ggplot(
        ggplot2::aes_string(
          x = x_pick,
          y = 'Number',
          colour = 'countriesAndTerritories',
          label = "data_point"
        )
      ) +
      { if (input$sel_window) {
        ggplot2::geom_rect(
          data = use_interventions2,
          ggplot2::aes_string(
            xmin = x_pick,
            xmax = x_pick2,
            ymin = 0,
            fill = "countriesAndTerritories",
            ymax = max(tab[["Number"]])
          ),
          colour = NA,
          alpha = 0.3
        )
      }} +
      ggplot2::geom_line(ggplot2::aes(linetype = Type)) +
      ggplot2::geom_rug(
        data = use_interventions,
        ggplot2::aes_string(
          x = x_pick,
          colour = "countriesAndTerritories",
          label = "data_point"
        ),
        inherit.aes = FALSE
      ) +
      ggplot2::labs(
        x = ifelse(input$sel_axis != "Date", "Days since measure introduced", "Date"),
        y = get_variable_name(input$sel_var, get_inter_variables())
      ) +
      ggplot2::scale_color_manual(values = country_colours) +
      ggplot2::scale_fill_manual(values = country_colours) +
      { if(x_pick == 'Date') {
        ggplot2::scale_x_date(breaks = '1 week', labels = scales::label_date("%d%b"))
      } else {
        ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10))
      }} +
      theme_shiny_dashboard() +
      { if(x_pick == 'Date') {
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }} +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
      ggplot2::theme(legend.position = "none") +
      { if(input$sel_smooth) ggplot2::geom_smooth(se = FALSE) }

    plotly::ggplotly(p, tooltip = c("label")) %>%
      plotly::layout(margin = list(l = 75))

  })


}
