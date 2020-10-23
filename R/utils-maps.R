ireland_map <- function(tab, title = "") {

  pal2 <- leaflet_map_pal(tab)

  tab %>%
    leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      leaflet::providers$Stamen.TonerLite,
      options = leaflet::providerTileOptions(noWrap = TRUE)
    ) %>%
    leaflet::setView(lng = -7.635498, lat = 53.186288, zoom = 6) %>%
    leaflet::addPolygons(
      stroke = FALSE,
      smoothFactor = 0.3,
      fillOpacity = 0.7,
      fillColor = ~ pal2(last14per100k),
      label = ~ paste0(CountyName, ": ", last14per100k, ' 14-day cases per 100k')
    ) %>%
    leaflet::addControl(title, position = "topright")
}

leaflet_map_pal <- function(tab) {
  leaflet::colorNumeric("Blues", tab$last14per100k)
}

trend_icon <- function(url = NULL, width = 15, height = 15) {
  if(is.null(url)) {
    url <- "https://cdn2.iconfinder.com/data/icons/font-awesome/1792/line-chart-512.png"
  }
  leaflet::makeIcon(
    iconUrl = url,
    iconWidth = width,
    iconHeight = height
  )
}

make_leaflet_popup_plots <- function(tab) {
  purrr::map(unique(tab$CountyName), leaflet_popup_plot, tab = tab)
}

leaflet_popup_plot <- function(tab, county) {

  tab <- tab %>%
    dplyr::filter(CountyName == county)

  latest_date <- max(tab$Date)

  number_cases <- tab %>%
    dplyr::filter(Date == latest_date) %>%
    dplyr::pull(last14per100k)

  tab %>%
    ggplot2::ggplot(ggplot2::aes(
      x = as.Date(Date),
      y = last14per100k,
      group = CountyName
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::labs(
      title = glue::glue("14-day cases per 100k in {county}"),
      subtitle = glue::glue("at {latest_date}: {number_cases}"),
      x = "Date",
      y = "14-day number of cases per 100k residents"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}


