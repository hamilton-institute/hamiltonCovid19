#' Creates the Ireland value boxes in the Summary tab
ireland_value_box <- function(tab, variable, title, icon) {

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
    status = "success",
    width = NULL,
    footer = create_update_message(tab_latest_data$Date)
  )
}
