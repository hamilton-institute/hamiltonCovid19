# Update and deploy app

args <- commandArgs(trailingOnly = TRUE)

deploy_app <- FALSE

# Update global_data
source("data-raw/global_data.R")

# Update irish_data
source("data-raw/irish_data.R")

# Update irish_county_data
source("data-raw/irish_county_data.R")

# Update interventions_data
# source("data-raw/interventions_data.R")

if (deploy_app) {

  remotes::install_deps(dependencies = TRUE)

  rsconnect::setAccountInfo(
    name = 'apmuhamilton',
    token = args[1],
    secret= args[2]
  )

  files <- list.files('.')
  files <- files[files != 'data-raw']

  rsconnect::deployApp(
    appFiles = files,
    appName = 'hamiltonCovid19Dashboard',
    forceUpdate = TRUE,
    account = 'apmuhamilton'
  )

}
