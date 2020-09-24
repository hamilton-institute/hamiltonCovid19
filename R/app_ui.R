#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    bs4Dash::bs4DashPage(
      navbar = create_navbar(),
      sidebar = create_sidebar(),
      body = create_body()
    )
  )
}

#' Creates app's navbar
create_navbar <- function() {
  bs4Dash::bs4DashNavbar(
    skin = "dark",
    status = "dark",
    border = TRUE,
    sidebarIcon = "bars",
    compact = FALSE,
    controlbarIcon = "th"
  )
}

#' Creates app's sidebar
create_sidebar <- function() {
  customBs4DashSidebar(
    skin = "dark",
    status = "danger",
    title = "Hamilton Institute",
    brandColor = "dark",
    url = "https://www.maynoothuniversity.ie/hamilton",
    src = "www/maynooth_university_logo.png",
    elevation = 1,
    opacity = 0.8,
    # bs4SidebarUserPanel(
    #   img = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
    #   text = "Welcome Onboard!"
    # ),
    bs4Dash::bs4SidebarMenu(
      bs4Dash::bs4SidebarMenuItem(
        "Summary",
        tabName = "summary",
        icon = "dashboard"
      ),
      bs4Dash::bs4SidebarMenuItem(
        "Map",
        tabName = "map",
        icon = "map"
      ),
      bs4Dash::bs4SidebarMenuItem(
        "Graphs",
        tabName = "graphs",
        icon = "bar-chart-o"
      ),
      bs4Dash::bs4SidebarMenuItem(
        "Animations",
        tabName = "animations",
        icon = "chart-line"
      ),
      bs4Dash::bs4SidebarMenuItem(
        "Interventions",
        tabName = "intervations",
        icon = "user-plus"
      ),
      bs4Dash::bs4SidebarMenuItem(
        "Sources",
        tabName = "sources",
        icon = "list-alt"
      )
    )
  )
}

#' Creates app's body
create_body <- function() {
  bs4Dash::bs4DashBody(
    bs4Dash::bs4TabItems(
      bs4Dash::bs4TabItem(
        tabName = "summary",
        mod_summary_ireland_ui("summary_ireland_ui_1"),
        mod_summary_global_ui("summary_global_ui_1")
      ),
      bs4Dash::bs4TabItem(
        tabName = "map",
        mod_map_ui("map_ui_1")
      ),
      bs4Dash::bs4TabItem(
        tabName = "graphs",
        mod_graphs_ui("graphs_ui_1")
      ),
      bs4Dash::bs4TabItem(
        tabName = "animations",
        mod_animations_ui("animations_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hamiltonCovid19'
    )
  )
}

