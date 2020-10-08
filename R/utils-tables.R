summaryTab_table <- function(tab) {

  col_spec <- purrr::map(
    c("left", "center"),
    ~ reactable::colDef(align = .x, width = 140)
  )

  col_spec <- purrr::set_names(col_spec, names(tab))

  reactable::reactable(
    tab,
    defaultPageSize = 10,
    searchable = TRUE,
    pagination = TRUE,
    rownames = TRUE,
    highlight = TRUE,
    paginationType = "simple",
    showPageInfo = FALSE,
    defaultColDef = reactable::colDef(
      align = "left",
      format = reactable::colFormat(separators = TRUE),
      width = 40
    ),
    columns = col_spec
  )

}

