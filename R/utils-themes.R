theme_shiny_dashboard <- function (base_size = 12, base_family = "") {
  ggplot2::theme_dark(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)), # 205 205 205
      axis.title = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)),
      axis.title.x = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)),
      axis.title.y = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)),
      plot.title = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)),
      legend.title = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)),
      legend.text = ggplot2::element_text(colour = rgb(205/255,205/255,205/255)),
      legend.background = ggplot2::element_rect(fill=rgb(70/255,80/255,90/255)),
      panel.background = ggplot2::element_rect(fill=rgb(70/255,80/255,90/255)), # 70 80 89
      plot.background = ggplot2::element_rect(fill=rgb(52/255,62/255,72/255)) # 52 62 72
    )
}

theme_bs4Dash <- function() {
  fresh::create_theme(
    fresh::bs4dash_layout(
      sidebar_width = "180px",
      main_bg = "#2d3741"
    ),
    fresh::bs4dash_sidebar_dark(
      bg = "#343e48"
    ),
    fresh::bs4dash_vars(
      navbar_dark_color = "#19232d",
      navbar_dark_active_color = "#46505a",
      navbar_dark_hover_color = "#46505a",
      navbar_brand_font_size = "0.9rem",
      navbar_toggler_padding_y = "0.94rem",
      link_color = "#3c8dbc"
    ),
    fresh::bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    fresh::bs4dash_status(
      dark = "#46505a",
      secondary = "#343e48"
    ),
    fresh::bs4dash_font(
      size_base = "0.9rem"
    ),
    fresh::bs4dash_color(
      gray_900 = "#FFF"
    )
  )
}

theme_reactable <- function() {
  reactable::reactableTheme(
    color = "#FFF",
    backgroundColor = "transparent",
    borderColor = "#343e48",
    stripedColor = "#FFF",
    highlightColor = "#46505a",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  )
}
