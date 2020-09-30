summaryTab_table <- function(tab) {

  reactable::reactable(
    tab,
    defaultPageSize = 10,
    searchable = TRUE,
    pagination = TRUE,
    rownames = TRUE,
    highlight = TRUE
  )

}

