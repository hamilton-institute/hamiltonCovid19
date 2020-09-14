#' Create text from Summary tab value boxes
create_value_box_text <- function(title, change) {
  paste0(
    title,
    br(),
    create_change_message(change)
  )
}

#' Creates change message for the value boxes
create_change_message <- function(value) {
  paste(
    html_arrow(value),
    ifelse(value == 0, 'No change', abs(value)),
    'since previous day'
  )
}

#' Returns a up, down or right arrow depending on the value
html_arrow <- function(value) {
  if (value == 0)  {
    fontawesome::fa(name = "arrow-right", fill = "grey")
  } else if (value < 0) {
    fontawesome::fa(name = "arrow-down", fill = "white")
  } else if (value > 0) {
    fontawesome::fa(name = "arrow-up", fill = "black")
  }
}

#' Creates "last updated" message for the value boxes
create_update_message <- function(date) {
  em("Updated: ", date, style = "font-size: 0.9vmax;")
}
