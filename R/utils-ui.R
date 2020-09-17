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
