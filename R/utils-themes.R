theme_shiny_dashboard <- function (base_size = 12, base_family = "") {

  default_text_color <- ggplot2::element_text(
    colour = status_para_cor("primary")
  )
  ggplot2::theme(
      axis.text = default_text_color,
      axis.title = default_text_color,
      axis.title.x = default_text_color,
      axis.title.y = default_text_color,
      plot.title = default_text_color,
      legend.title = default_text_color,
      legend.text =default_text_color,
      legend.background = ggplot2::element_rect(
        fill = "white"
      ),
      panel.background = ggplot2::element_rect(
        fill = "white"
      ),
      plot.background = ggplot2::element_rect(
        fill = "white"
      )
    )
}

#' create_theme_css
#'
#' @export
#'
#' @import fresh
theme_bs4Dash <- function() {
  fresh::create_theme(
    fresh::bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#ffffff"
    ),
    fresh::bs4dash_status(
      info = status_para_cor("info"),
      secondary = status_para_cor("secondary"),
      primary = status_para_cor("primary"),
      success = status_para_cor("success"),
      warning = status_para_cor("warning"),
      danger = status_para_cor("danger"),
      light = status_para_cor("primary")
    ),
    fresh::bs4dash_color(
      lightblue = status_para_cor("info"),
      gray_800 = "#164b53",
      blue = status_para_cor("primary"),
      green = status_para_cor("success"),
      yellow = status_para_cor("warning")
    ),
    fresh::bs4dash_sidebar_light(
      bg = status_para_cor("secondary")
    ),
    fresh::bs4dash_layout(
      main_bg = "#ffffff",
      sidebar_width = "220px"
    ),
    fresh::bs4dash_vars(
      body_color = status_para_cor("primary")
    )
  )
}

status_para_cor <- function(status) {
  switch (status,
          primary = "#164b53",
          secondary = colorspace::lighten("#164b53", 0.99),
          info = "#e9ecef",
          success = "#046874",
          warning = "#b99306",
          danger = "#bf281e")
}

theme_reactable <- function() {
  reactable::reactableTheme(
    color = status_para_cor("primary"),
    backgroundColor = "transparent",
    borderColor = "#343e48",
    stripedColor = status_para_cor("success"),
    highlightColor = status_para_cor("info"),
    inputStyle = list(backgroundColor = "white"),
    selectStyle = list(backgroundColor = "white"),
    pageButtonHoverStyle = list(
      backgroundColor = status_para_cor("info")
    ),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  )
}
