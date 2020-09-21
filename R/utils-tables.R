summaryTab_table <- function(tab) {
  DT::datatable(
    tab,
    options = list(
      pageLength = 10,
      searching = TRUE,
      paging = TRUE,
      autoWidth = TRUE,
      rowNames = TRUE
    )) %>%
    DT::formatStyle(columns = 1, color = "#c8c8c8", target = "row")
}
