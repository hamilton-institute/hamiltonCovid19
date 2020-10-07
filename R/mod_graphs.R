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
      col_3(
        offset = 1,
        uiOutput(ns("ui_sel_ctry"))
      ),
      col_3(
        shinyWidgets::pickerInput(
          inputId = ns("sel_var"),
          label = "Select variables",
          choices = get_graph_variables(),
          selected = c('deaths_per_million'),
          multiple = TRUE
        )
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
          selected = c('Days since 1st death'),
          multiple = FALSE
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

  output$ui_sel_ctry <- renderUI({
    global_data %>%
      dplyr::pull(countriesAndTerritories) %>%
      unique() %>%
      country_picker(id = ns("sel_ctry"))
  })

  output$CountryPlot <- plotly::renderPlotly({

    validate(
      need(
        length(input$sel_ctry) > 0,
        "Please select some countries. Use Global for worldwide values.")
    )

    global_agg <- global_data %>%
      dplyr::filter(countriesAndTerritories %in% input$sel_ctry) %>%
      dplyr::select(
        Date,
        cases,
        deaths,
        countriesAndTerritories,
        popData2019
      ) %>%
      dplyr::group_by(countriesAndTerritories) %>%
      dplyr::arrange(Date) %>%
      dplyr::mutate(
        cum_cases = cumsum(cases),
        cum_deaths = cumsum(deaths),
        cases_per_million = 1e6 * cumsum(cases) / popData2019,
        deaths_per_million = 1e6 * cumsum(deaths) / popData2019
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        dplyr::across(
          c("cases", "deaths", "cum_cases", "cum_deaths"),
          ~ .x + 1,
          .names = "log_{col}"
        )
      )

    create_color_pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))

    country_colours <-  global_agg %>%
      dplyr::arrange(desc(popData2019)) %>%
      dplyr::distinct(countriesAndTerritories) %>%
      dplyr::mutate(
        Colours = create_color_pal(dplyr::n())
      ) %>%
      tibble::deframe()

    x_pick <- switch(
      input$sel_axis,
      'Date' = 'Date',
      'days_since'
    )

    if (input$sel_axis == 'Days since 1st case' | input$sel_axis == 'Date') {
      global_agg = global_agg %>% dplyr::filter(cum_cases > 0)
    } else if (input$sel_axis == 'Days since 1st death') {
      global_agg = global_agg %>% dplyr::filter(cum_deaths > 0)
    } else if (input$sel_axis == 'Days since 10th death') {
      global_agg = global_agg %>% dplyr::filter(cum_deaths >= 10)
    } else if (input$sel_axis == 'Days since 10th case') {
      global_agg = global_agg %>% dplyr::filter(cum_cases >= 10)
    }

    global_agg <- global_agg %>%
      dplyr::group_by(countriesAndTerritories) %>%
      dplyr::mutate(days_since = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        names_to = 'Type',
        values_to = 'Number',
        -c(Date, countriesAndTerritories, popData2019, days_since)
      )

    global_agg <- global_agg %>%
      dplyr::filter(Type %in% input$sel_var)

    global_agg <- global_agg %>%
      dplyr::mutate(
        data_point = paste0(
          "\ncountry: ",
          global_agg$countriesAndTerritories,
          "\nx_axis: ",
          global_agg[[x_pick]],
          "\n",
          "y_axis: ",
          formatC(
            signif(Number,digits = 3),
            digits = 3,
            format = "fg",
            flag = "#"
          )
        )
      )

    n_countries <- dplyr::n_distinct(global_agg$countriesAndTerritories)

    n_vars <- length(input$sel_var)

    p <- ggplot2::ggplot(
      global_agg,
      ggplot2::aes_string(
        x = x_pick,
        y = 'Number',
        colour = 'countriesAndTerritories',
        label = "data_point"
      )
    ) +
      ggplot2::geom_line(ggplot2::aes(linetype = Type)) +
      ggplot2::labs(
        x = "", #input$sel_axis,
        y = "" # paste(input$sel_var, collapse = ',')
      ) +
      ggplot2::scale_color_manual(values = country_colours) +
      { if(x_pick == 'Date') {
        ggplot2::scale_x_date(
          breaks = '1 week',
          labels = scales::label_date("%d%b")
        )
      } else {
        ggplot2::scale_x_continuous(
          breaks =  scales::breaks_pretty(n = 10),
          labels = scales::comma
        )
      }} +
      theme_shiny_dashboard() +
      { if(x_pick == 'Date') ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) } +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      {if (all(stringr::str_detect(input$sel_var,'Logp1')))  {
        ggplot2::scale_y_continuous(trans = scales::log1p_trans(),
                           labels = scales::comma,
                           breaks = scales::breaks_log(n = 5))
      } else {
        ggplot2::scale_y_continuous(labels = scales::comma,
                           breaks = scales::breaks_pretty(n = 5))
      }} +
      { if(n_countries * n_vars > 10) ggplot2::theme(legend.position = "none")}

    plotly::ggplotly(p, tooltip = c("label")) %>%
      plotly::layout(margin = list(l = 75))
    #ggplotly(p) %>% layout(margin = list(l = 75))

  })

}
