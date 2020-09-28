ani_graph <- function(tab, ctry, x_pick, y_pick, date) {

  global_agg <- tab %>%
    dplyr::filter(countriesAndTerritories %in% ctry) %>%
    dplyr::select(
      Date,
      cases,
      deaths,
      countriesAndTerritories,
      popData2019,
      day,
      month
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


interv_graph <- function() {

}
