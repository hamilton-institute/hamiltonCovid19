ireland_map <- function(tab) {

  pal2 <- leaflet::colorNumeric("Blues", log2(tab$ConfirmedCovidCases))


  leaflet::leaflet(tab) %>%
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
