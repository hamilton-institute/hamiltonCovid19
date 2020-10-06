#' Creates the Ireland value boxes in the Summary tab
value_box_counts <- function(tab, variable, title, icon, status = "success") {

  tab_latest_data <- tab %>%
    dplyr::filter(Date == max(Date))

  latest_val <- dplyr::pull(tab_latest_data, {{variable}})

  previous_val <- tab %>%
    dplyr::filter(Date == max(Date) - lubridate::days(1)) %>%
    dplyr::pull({{variable}})

  change <- latest_val - previous_val

  val <- stringr::str_pad(
    string = format(latest_val, big.mark = ','),
    width = 9,
    side = 'right'
  )

  text <- create_value_box_text(
    title = title,
    change = change
  )

  bs4Dash::bs4ValueBox(
    value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
    subtitle = tags$p(HTML(text)),
    icon = icon,
    status = status,
    width = NULL,
    footer = create_update_message(tab_latest_data$Date)
  )
}

value_box_countries <- function(tab, variable, title, icon) {

  name <- stringr::str_replace_all(
    tab$countriesAndTerritories[1], '_', ' '
  )

  value <- tab %>%
    dplyr::slice(1) %>%
    dplyr::pull({{variable}}) %>%
    abs() %>%
    format(big.mark = ',')

  bs4Dash::bs4ValueBox(
    value = tags$p(
      name,
      style = paste0(
        "font-size: ",
        ifelse(nchar(name) < 10, 3, 3 * 9 / nchar(name)),
        "vmax; ",
        "color: black;"
      )
    ),
    subtitle = p(HTML(paste0(title, value)), style = "color: black;"),
    status = 'secondary',
    icon = icon
  )
}

function(x) {
  switch(
    input$sel_var[x],
    'Cumulative cases' = 'cum_cases',
    'Cumulative deaths' = 'cum_deaths',
    'Daily cases' = 'cases',
    'Daily deaths' = 'deaths',
    'Logp1 cumulative cases' = 'cum_cases',
    'Logp1 cumulative deaths' = 'cum_deaths',
    'Logp1 daily cases' = 'cases',
    'Logp1 daily deaths' = 'deaths',
    'Cases per million population' = 'cases_per_million',
    'Deaths per million population' = 'deaths_per_million'
  )
}
