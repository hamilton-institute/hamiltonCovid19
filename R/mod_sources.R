#' sources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sources_ui <- function(id){
  ns <- NS(id)
  tagList(
    helpText(
      h4("Data sources:"),
      sources_item(
        link = "https://www.ecdc.europa.eu/en",
        icon = "hospital",
        title = "ECDC"
      ),
      sources_item(
        link = "https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/",
        icon = "university",
        title = "Irish government data"
      ),
      sources_item(
        link = "https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports",
        icon = "university",
        title = "Northern Ireland government data"
      ),
      sources_item(
        link = "https://data.humdata.org/dataset/e1a91ae0-292d-4434-bc75-bf863d4608ba/resource/93108e8e-8afc-4f26-950b-0c1e587ee5c2/download/20200416-acaps-covid-19-goverment-measures-dataset-v8.xlsx",
        icon = "user-plus",
        title = "Interventions data"
      ),
      br(),
      p("ECDC data are updated for the next day usually around 12pm, with sporadic updates occuring at other times. Irish goverment provisional figures are updated daily at around 6pm with confirmed figures given for two days previous."),
      p("Irish hospitalistion statistics are only given in the confirmed figures so are slightly older than the provisional data."),
      p("The Irish confirmed figures are significantly higher than the corresponding estimates given for those days by the ECDC."),
      p("The Northern Ireland figures are given by district which does not match precisely into counties. We have made an arbitrary decision as to which district are allocated to which counties. Details are provided in the GitHub repository in the Irish data spreadsheet"),
      br(),
      h6(
        fa_icon(name = "users", fill = "#FFFFFF", height = "20px"),
        'Contributors:', a("GitHub contributors page", href= "https://github.com/hamilton-institute/covid19ireland/graphs/contributors")
      ),
      h6(
        fa_icon(name = "bug", fill = "#FFFFFF", height = "20px"),
        'Report bugs and suggest features at the', a("Github issues page", href= "https://github.com/hamilton-institute/covid19ireland/issues")
      ),
      h6(
        fa_icon(name = "github", fill = "#FFFFFF", height = "20px"),
        'See the code on ', a("Github", href= "https://github.com/hamilton-institute/covid19ireland")
      )
    )
  )
}
