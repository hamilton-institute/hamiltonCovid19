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

#' Create distill theme for bs4Dash
#'
#' @export
theme_bs4Dash_distill <- function() {
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
      sidebar_width = "180px"
    ),
    fresh::bs4dash_vars(
      body_color = status_para_cor("primary")
    )
  )
}


#' Create distill theme for bs4Dash
#'
#' @return
#' @export
theme_bs4Dash <- function() {
  theme_bs4Dash_distill()
}

#' Use bs4Dash distill theme css
#'
#' @export
use_css_bs4Dash_distill <- function() {
  file.copy(
    from = system.file("app/www/custom.css", package = "hamiltonCovid19"),
    to = "app/www/custom.css"
  )
}



#' color pallete
#'
#' @export
status_para_cor <- function(status) {
  switch (status,
          primary = "#164b53",
          secondary = colorspace::lighten("#164b53", 0.99),
          info = "#e9ecef",
          success = "#046874",
          warning = "#b99306",
          danger = "#bf281e")
}


#' Spinner
#'
#' @export
#'
with_load_spinner <- function(ui_element, type = 4, color = status_para_cor("primary"), ...) {
  shinycssloaders::withSpinner(ui_element, type = type, color = color, ...)
}

theme_reactable <- function() {
  reactable::reactableTheme(
    color = status_para_cor("primary"),
    backgroundColor = status_para_cor("secondary"),
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

create_color_pal <- function(n) {
  colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(n)
}
