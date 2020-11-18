## Update `irish_hosp_data` dataset goes here

usethis::ui_todo("Checking update for irish_data dataset...")

`%>%` <- magrittr::`%>%`

url_hosp <- "https://services-eu1.arcgis.com/z6bHNio59iTqqSUY/arcgis/rest/services/Covid19AcuteHospitalHistoricSummaryOpenData/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json"

res_hosp <- tryCatch(
  httr::GET(url_hosp),
  error = function(x) NULL
)

raw_irish_hosp_data <- res_hosp %>%
  httr::content() %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("features", "attributes")

if (is.null(raw_irish_hosp_data)) {
  usethis::ui_oops(paste0("Failed to connect to ", url_hosp))
} else {
  # Testing new dataset
  old_irish_hosp_data <- readr::read_rds("data-raw/rds/raw_irish_hosp_data.rds")

  tab_duplicates <- raw_irish_hosp_data %>%
    dplyr::count(Date) %>%
    dplyr::filter(n > 1)

  needed_columns <- c(
    "Date",
    "SUM_number_of_confirmed_covid_1",
    "SUM_no_new_admissions_covid19_p",
    "SUM_no_discharges_covid19_posit"
  )

  missing_columns <- needed_columns[
    !needed_columns %in% names(raw_irish_hosp_data)
  ]

  if (identical(old_irish_hosp_data, raw_irish_hosp_data) |
      nrow(old_irish_hosp_data) > nrow(raw_irish_hosp_data)) {
    usethis::ui_done("Nothing to update in irish_hosp_data")
  } else if (nrow(tab_duplicates) > 0) {
    usethis::ui_oops("New data has duplicated Date entries. Dataset not updated.")
  } else if (length(missing_columns) > 0) {
    usethis::ui_oops(paste0(
      "The following columns are missing in the new dataset: ",
      paste(missing_columns, collapse = ", "),
      ". Dataset not updated."
    ))
  } else {
    # Updating dataset
    suppressMessages({
      readr::write_rds(
        raw_irish_hosp_data,
        "data-raw/rds/raw_irish_hosp_data.rds",
        compress = "xz"
      )

      irish_hosp_data <- raw_irish_hosp_data %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
          DateTime = lubridate::as_datetime(Date/1000),
          DateTime = DateTime - lubridate::hours(3), # fixing time difference
          Date = lubridate::as_date(DateTime)
        ) %>%
        dplyr::select(
          Date,
          DateTime,
          currentHospitalisedCases = SUM_number_of_confirmed_covid_1,
          last24HospAdmissions = SUM_no_new_admissions_covid19_p,
          last24HospDischarges = SUM_no_discharges_covid19_posit
        )

      deploy_app <- TRUE

      usethis::use_data(irish_hosp_data, overwrite = TRUE)
    })
    usethis::ui_done("irish_hosp_data dataset updated!")
  }
}
