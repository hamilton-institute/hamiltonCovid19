create_value_box_text <- function(title, change) {
  paste0(
    title,
    br(),
    create_change_message(change)
  )
}

create_change_message <- function(value) {
  paste(
    html_arrow(value),
    ifelse(value == 0, 'No change', format_number(value)),
    'since previous day'
  )
}

html_arrow <- function(value) {
  if (value == 0)  {
    fa_icon(name = "arrow-right", fill = "#acabb0")
  } else if (value < 0) {
    fa_icon(name = "arrow-down", fill = "white")
  } else if (value > 0) {
    fa_icon(name = "arrow-up", fill = "black")
  }
}

create_update_message <- function(date) {
  em("Updated: ", date, style = "font-size: 0.9vmax;")
}

fa_icon <- function(name, height = NULL, fill = NULL, top = "-0.1em") {

  full_name <- NULL

  if (name %in% fontawesome:::fa_tbl$full_name) {
    svg <- fontawesome:::fa_tbl[which(
      fontawesome:::fa_tbl$full_name == name
    ), ][1, 4]
  }
  else if (name %in% fontawesome:::fa_tbl$name) {
    svg <- fontawesome:::fa_tbl[which(
      fontawesome:::fa_tbl$name == name
    ), ][1, 4]
  }
  else {
    return("")
  }

  style <- "style=\""

  if (is.null(height)) {
    style <- paste0(style, "height:0.8em;position:relative;")
  }
  else {
    style <- paste0(style, "height:", height, ";")
  }

  if (is.null(top)) {
    style <- paste0(style, "top: -0.1em;")
  } else {
    style <- paste0(style, "top:", top, ";")
  }

  if (!is.null(fill)) {
    style <- paste0(style, "fill:", fill, ";")
  }

  style <- paste0(style, "\"")

  if (!grepl(style, pattern = "style=\"\"")) {
    svg <- gsub(pattern = "^<svg", replacement = paste0("<svg ",
                                                        style), x = svg)
  }

  svg <- htmltools::HTML(svg)
  class(svg) <- c("fontawesome", class(svg))

  svg

}

sources_item <- function(link, icon, title) {
  tagList(
    a(
      style = "margin-bottom: 13px;",
      href = link, target = "_blank",
      h5(
        fa_icon(
          name = icon,
          fill = status_para_cor("primary"),
          height = "20px"
        ),
        title
      )
    )
  )
}
