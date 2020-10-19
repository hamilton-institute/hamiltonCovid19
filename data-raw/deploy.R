files <- list.files(".")
files <- files[files != "data-raw"]

rsconnect::setAccountInfo(
  name = "apmuhamilton",
  token = ${{secrets.SHINYAPPS_TOKEN}},
  secret = ${{secrets.SHINYAPPS_SECRET}}
)

rsconnect::deployApp(
  appDir = ".",
  appFiles = files,
  appName = "hamiltonCovid19Dashboard",
  forceUpdate = TRUE,
  account = "apmuhamilton"
)
