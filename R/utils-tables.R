summaryTab_table <- function(tab) {

  col_spec <- purrr::map2(
    c("left", "left", "center"),
    c(100, 80, 80),
    ~ reactable::colDef(align = .x, minWidth = .y, maxWidth = 200)
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
      maxWidth = 40
    ),
    columns = col_spec
  )

}

