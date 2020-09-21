ireland_map <- function(tab) {

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
      fillColor = ~ pal2(log2(ConfirmedCovidCases)),
      label = ~ paste0(CountyName, ": ", ConfirmedCovidCases, ' cases')
    )
}

leaflet_map_pal <- function(tab) {
  leaflet::colorNumeric("Blues", log2(tab$ConfirmedCovidCases))
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
  purrr::map(tab$NAME_TAG, leaflet_popup_plot, tab = tab)
}

leaflet_popup_plot <- function(tab, county) {

  latest_date <- max(tab$Date)
  number_cases <- tab %>%
    dplyr::filter(Date == latest_date) %>%
    dplyr::pull(`Number of Cases`)

  tab %>%
    filter(CountyName == county) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = as.Date(Date),
      y = `Number of Cases`,
      group = CountyName
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::labs(
      title = glue::glue(
        "Total cases in {county} at {latest_date}: {number_cases}"
      ),
      x = "Date",
      y = "Number of individuals"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90))
}


