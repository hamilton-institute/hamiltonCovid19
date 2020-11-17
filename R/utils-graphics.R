ani_graph <- function(tab, ctry, x_pick, y_pick, date) {

  global_agg <- tab %>%
    dplyr::filter(countriesAndTerritories %in% ctry) %>%
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
      daily_cases = cases,
      daily_deaths = deaths,
      cases_per_million = 1e6 * cumsum(cases) / popData2019,
      deaths_per_million = 1e6 * cumsum(deaths) / popData2019
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(
        c("daily_cases", "daily_deaths", "cum_cases", "cum_deaths"),
        ~ .x + 1,
        .names = "log_{col}"
      ),
      dplyr::across(
        c("daily_cases", "daily_deaths", "cum_cases", "cum_deaths"),
        ~ .x,
        .names = "sqrt_{col}"
      )
    )

  validate(
    need(
      nrow(global_agg) > 0,
      "Please select some countries. Use Global for worldwide values."
    )
  )

  create_pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))

  country_colours2 <- global_agg %>%
    dplyr::arrange(desc(popData2019)) %>%
    dplyr::distinct(countriesAndTerritories) %>%
    dplyr::mutate(
      Colours = create_pal(dplyr::n())
    ) %>%
    tibble::deframe()

  global_agg <- global_agg %>%
    dplyr::mutate(
      countriesAndTerritories = readr::parse_factor(countriesAndTerritories),
      month_day = format(Date, format = "%b-%d")) %>%
    dplyr::select(
      "Date",
      x_pick,
      y_pick,
      "countriesAndTerritories",
      'month_day'
    )

  global_agg %>%
    dplyr::filter(Date == date) %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        x_pick,
        y_pick,
        colour = "countriesAndTerritories",
        size = y_pick
      )
    ) +
    ggplot2::scale_color_manual(values = country_colours2) +
    ggplot2::annotation_custom(
      grid::textGrob(
        global_agg$month_day[match(date, global_agg$Date)],
        gp = grid::gpar(fontsize=200, col="grey")
      ),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) +
    ggplot2::geom_point(data = dplyr::filter(global_agg, Date <= date)) +
    ggplot2::geom_line(data = dplyr::filter(global_agg, Date <= date), alpha = 0.4) +
    ggplot2::geom_label(ggplot2::aes(label = countriesAndTerritories)) +
    ggplot2::labs(
      x = get_variable_name(x_pick, get_anim_variables()),
      y = get_variable_name(y_pick, get_anim_variables())
    ) +
    ggplot2::scale_size(range = c(2, 12)) +
    { if (stringr::str_detect(x_pick, 'sqrt')) {
      ggplot2::scale_x_sqrt(
        breaks = scales::breaks_pretty(n = 7),
        labels = scales::comma,
        limits = c(min(global_agg[[x_pick]]), max(global_agg[[x_pick]]))
      )
    } else if(stringr::str_detect(x_pick, 'log')) {
      ggplot2::scale_x_continuous(
        trans = scales::log1p_trans(),
        labels = scales::comma,
        breaks = scales::breaks_log(n = 7),
        limits = c(min(global_agg[[x_pick]]), max(global_agg[[x_pick]]))
      )
    } else {
      ggplot2::scale_x_continuous(
        labels = scales::comma,
        breaks = scales::breaks_pretty(n = 7),
        limits = c(min(global_agg[[x_pick]]), max(global_agg[[x_pick]]))
      )
    }} +
    { if (stringr::str_detect(y_pick, 'sqrt')) {
      ggplot2::scale_y_sqrt(
        labels = scales::comma,
        breaks = scales::breaks_pretty(n = 7),
        limits = c(min(global_agg[[y_pick]]), max(global_agg[[y_pick]]))
      )
    } else if(stringr::str_detect(y_pick,'log')) {
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        trans = scales::log1p_trans(),
        breaks = scales::breaks_log(n = 7),
        limits = c(min(global_agg[[y_pick]]), max(global_agg[[y_pick]]))
      )
    } else {
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        breaks = scales::breaks_pretty(n = 7),
        limits = c(min(global_agg[[y_pick]]), max(global_agg[[y_pick]]))
      )
    }} +
    theme_shiny_dashboard() +
    ggplot2::theme(
      legend.position = 'None',
      axis.title.y = ggplot2::element_text(angle = 0, vjust = 1, hjust=0)
    )

}


interv_graph <- function(tab) {

  tab <- tab %>%
    dplyr::filter(countriesAndTerritories %in% input$sel_ctry) %>%
    dplyr::select(
      Date,
      cases,
      deaths,
      totalCases,
      totalDeaths,
      logTotalCases,
      logTotalDeaths,
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
}

graphs_tab_plot <- function(tab, variables, x_pick) {

  n_countries <- dplyr::n_distinct(tab$countriesAndTerritories)
  n_vars <- length(variables)

  if (n_countries > 1 | n_vars == 1) {
    graphs_tab_plot_(tab, "countriesAndTerritories", variables, x_pick)
  } else {
    graphs_tab_plot_(tab, "Type", variables, x_pick)
  }

}

graphs_tab_plot_ <- function(tab, color_var, variables, x_pick) {

  if (color_var == "countriesAndTerritories") {
    colours <-  pick_colors(tab, popData2019, countriesAndTerritories)
  } else {
    colours <-  pick_colors(tab, popData2019, Type)
  }

  if(x_pick == 'Date') {
    x_scale <- ggplot2::scale_x_date(
      breaks = '2 weeks',
      labels = scales::label_date("%d%b")
    )

    x_text_theme <- ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  } else {
    x_scale <- ggplot2::scale_x_continuous(
      breaks =  scales::breaks_pretty(n = 10),
      labels = scales::comma
    )

    x_text_theme <- ggplot2::theme()
  }

  y_lab <- ifelse(
    color_var == "Type",
    "Value",
    get_variable_name(variables, get_graph_variables())
  )

  p <- tab %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        x = x_pick,
        y = "Value",
        colour = color_var,
        label = "data_point",
        group = color_var
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      y = y_lab,
      x = names(x_pick),
      color = ""
    ) +
    ggplot2::scale_color_manual(values = colours) +
    x_scale +
    theme_shiny_dashboard() +
    x_text_theme +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      breaks = scales::breaks_pretty(n = 5)
    )

  plotly::ggplotly(p, tooltip = c("label")) %>%
    plotly::layout(margin = list(l = 75))
}
