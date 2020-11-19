#' Creates the Ireland value boxes in the Summary tab
value_box_counts <- function(tab, variable, title, icon, status = "success") {

  tab_latest_data <- tab %>%
    dplyr::filter(Date == max(Date))

  latest_val <- dplyr::pull(tab_latest_data, {{variable}})

  previous_val <- tab %>%
    dplyr::filter(Date == max(Date) - lubridate::days(1)) %>%
    dplyr::pull({{variable}})

  change <- latest_val - previous_val

  # val <- stringr::str_pad(
  #   string = format(latest_val, big.mark = ','),
  #   width = 9,
  #   side = 'right'
  # )

  val_text <- paste(format(latest_val, big.mark = ','), title)

  text <- create_value_box_text(
    title = paste0("Total number of ", title),
    change = change
  )

  bs4Dash::bs4ValueBox(
    value = tags$p(val_text, style = "font-size: 2vmax; margin-bottom: 0;"),
    subtitle = tags$p(HTML(text)),
    icon = icon,
    status = status,
    width = NULL,
    footer = create_update_message(tab_latest_data$Date)
  )
}

value_box_current_vs_max <- function(tab, variable, var_name, icon, status = "success") {

  tab_latest_date <- tab %>%
    dplyr::filter(!is.na({{variable}})) %>%
    dplyr::filter(Date == max(Date))

  latest_updated_dateTime <- format(
    max(tab_latest_date$DateTime), "%Y-%m-%d at %H:%M"
  )

  tab_max <- tab %>%
    dplyr::filter({{variable}} == max({{variable}})) %>%
    dplyr::slice_max(n = 1, order_by = Date)

  max_value <- tab_max %>%
    dplyr::pull({{variable}})

  max_date <- format(tab_max$Date, "%B %d, %Y")

  latest_val <- tab_latest_date %>%
    dplyr::pull({{variable}})

  val_text <- paste(format(latest_val, big.mark = ','), var_name)


  if (latest_val < max_value) {
    max_text <- paste0(
      "Max was ",
      format(max_value, big.mark = ','),
      " on ",
      max_date
    )
  } else {
    max_text <- paste0(
      format(latest_val, big.mark = ','),
      " is the new maximum"
    )
  }

  previous_val <- tab %>%
    dplyr::filter(Date == max(Date) - lubridate::days(1)) %>%
    dplyr::pull({{variable}})

  change <- latest_val - previous_val

  text <- create_value_box_text(
    title = max_text,
    change = change
  )

  bs4Dash::bs4ValueBox(
    value = tags$p(val_text, style = "font-size: 2vmax; margin-bottom: 0;"),
    subtitle = tags$p(HTML(text)),
    icon = icon,
    status = status,
    width = NULL,
    footer = create_update_message(latest_updated_dateTime)
  )
}

value_box_countries <- function(tab, variable, title, icon) {

  name <- stringr::str_replace_all(
    tab$countriesAndTerritories[1], '_', ' '
  )

  value <- tab %>%
    dplyr::slice(1) %>%
    dplyr::pull({{variable}}) %>%
    format_number()

  bs4Dash::bs4ValueBox(
    value = tags$p(
      name,
      style = paste0(
        "font-size: ",
        ifelse(nchar(name) < 10, 3, 3 * 9 / nchar(name)),
        "vmax; ",
        "color: ",
        status_para_cor("primary"),
        ";"
      )
    ),
    subtitle = p(
      HTML(paste0(title, value)),
      style = paste("color: ", status_para_cor("primary"), ";")
    ),
    status = 'secondary',
    icon = icon
  )
}

pick_colors <- function(tab, ordering_col, grouping_col) {
  tab %>%
    dplyr::arrange(desc({{ordering_col}})) %>%
    dplyr::distinct({{grouping_col}}) %>%
    dplyr::mutate(
      Colours = create_color_pal(dplyr::n())
    ) %>%
    tibble::deframe()
}

create_trigger_value <- function(x, value) {
  purrr::set_names(
    paste(x, value, sep = "@"),
    names(x)
  )
}

remove_trigger_value <- function(x) {
  stringr::str_remove(x, "@.*$")
}

format_number <- function(value) {
  value %>%
    abs() %>%
    round(1) %>%
    format(big.mark = ",")
}

format_decimal_number <- function(x, numeric = TRUE) {
  values <- formatC(
    signif(x, digits = 3),
    digits = 3,
    format = "fg",
    flag = "#"
  )
  if (numeric) {
    as.numeric(values)
  } else {
    values
  }
}
