#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...){
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_7 <- function(...){
  column(7, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}

#' @importFrom shiny column
col_5 <- function(...){
  column(5, ...)
}


#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}


#' Custom bs4DashSidebar function, changing target option in
#' brandTag to "_blank"
customBs4DashSidebar <- function (..., inputId = NULL, disable = FALSE,
                                  title = NULL, skin = "dark",
                                  status = "primary", brandColor = NULL,
                                  url = NULL, src = NULL, elevation = 4,
                                  opacity = 0.8, expand_on_hover = TRUE) {

  brandTag <- if (!is.null(title)) {

    shiny::tags$a(
      class = if (!is.null(brandColor)) paste0("brand-link bg-", brandColor) else "brand-link",
      href = url, shiny::tags$img(
        src = src,
        class = "brand-image img-circle elevation-3",
        style = paste0("opacity: ", opacity)
      ),
      target = "_blank",
      shiny::tags$span(class = "brand-text font-weight-light",title)
    )
  }

  contentTag <- shiny::tags$div(
    class = "sidebar",
    shiny::tags$nav(class = "mt-2", ...)
  )

  sidebarTag <- shiny::tags$aside(
    id = inputId,
    class = paste0(
      "main-sidebar sidebar-",
      skin,
      "-",
      status,
      " elevation-",
      elevation,
      if (expand_on_hover) NULL else " sidebar-no-expand"
    ),
    style = if (disable) "display: none;"
  )

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, brandTag, contentTag)
  sidebarTag

  customCSS <- shiny::singleton(
    shiny::tags$style(
      ".content-wrapper, .main-footer, .main-header {\n margin-left: 0px;\n }\n"
    )
  )

  if (disable)
    shiny::tagList(customCSS, sidebarTag)
  else
    sidebarTag

}

get_inter_variables <- function() {
  c(
    'Cumulative cases' = 'totalCases',
    'Cumulative deaths' = 'totalDeaths',
    'Daily cases' = 'cases',
    'Daily deaths' = 'deaths',
    'Log cumulative cases' = 'logp1TotalCases',
    'Log cumulative deaths' = 'logp1TotalDeaths',
    'Cases per million population' = 'casesPerMillion',
    'Deaths per million population' = 'deathsPerMillion'
  )
}

get_graph_variables <- function() {
  c(
    "14-days cases per 100k residents" = "totalCases14Days",
    'Cumulative cases' = 'totalCases',
    'Cumulative deaths' = 'totalDeaths',
    'Daily cases' = 'cases',
    'Daily deaths' = 'deaths',
    'Logp1 cumulative cases' = 'logp1TotalCases',
    'Logp1 cumulative deaths' = 'logp1TotalDeaths',
    'Logp1 daily cases' = 'logp1Cases',
    'Logp1 daily deaths' = 'logp1Deaths',
    'Cases per million population' = 'casesPerMillion',
    'Deaths per million population' = 'deathsPerMillion'
  )
}

get_anim_variables <- function() {
  c(
    'Cumulative cases' = 'cum_cases',
    'Cumulative deaths' = 'cum_deaths',
    'Daily cases' = 'daily_cases',
    'Daily deaths' = 'daily_deaths',
    'Logp1 daily cases' = 'log_daily_cases',
    'Logp1 daily deaths' = 'log_daily_deaths',
    'Sqrt daily cases' = 'sqrt_daily_cases',
    'Sqrt daily deaths' = 'sqrt_daily_deaths',
    'Sqrt cumulative cases' = 'sqrt_cum_cases',
    'Sqrt cumulative deaths' = 'sqrt_cum_deaths',
    'Logp1 cumulative cases' = 'log_cum_cases',
    'Logp1 cumulative deaths' = 'logcum_deaths',
    'Cumulative cases per million population' = 'cases_per_million',
    'Cumulative deaths per million population' = 'deaths_per_million'
  )
}

get_summary_variables <- function() {
  c(
    "14-days cases per 100k residents" = "totalCases14Days",
    'Daily cases' = "cases",
    "Total cases" = "totalCases",
    "Cases increased since yesterday" = "changeCases",
    "Deaths increased since yesterday" = "changeDeaths"
  )
}

get_variable_name <- function(x, vars, remove_log_sqrt = TRUE) {
  var_names <- tibble::tibble(value = x) %>%
    dplyr::left_join(tibble::enframe(vars), by = "value") %>%
    dplyr::pull(name)

  if(remove_log_sqrt) {
    var_names %>%
      stringr::str_remove("Sqrt") %>%
      stringr::str_remove("Logp1") %>%
      stringr::str_to_sentence()
  } else {
    var_names
  }
}

default_countries <- function() {
  c(
    'France',
    'Ireland',
    'UK',
    'USA',
    'Spain',
    'Belgium',
    'Italy'
  )
}

country_picker <- function(choices, id) {
  shinyWidgets::pickerInput(
    inputId = id,
    label = "Select countries",
    choices = choices,
    selected = default_countries(),
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      size = 10
    ),
    multiple = TRUE,
    width = "100%"
  )
}

custom_box <- function(title, width, ...) {
  bs4Dash::box(
    title = title,
    width = width,
    gradientColor = "secondary",
    closable = FALSE,
    collapsible = FALSE,
    ... = ...
  )
}
