## Update irish_data dataset

usethis::ui_todo("Checking update for irish_data dataset...")

`%>%` <- magrittr::`%>%`

url_irl <- "https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.geojson"

raw_irish_data <- tryCatch(
  url_irl %>%
    RCurl::getURL() %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "properties"),
  error = function(x) NULL
)

if (is.null(raw_irish_data)) {
  usethis::ui_oops(paste0("Failed to connect to ", url_irl))
} else {
  # Testing new dataset
  old_irish_data <- readr::read_rds("data-raw/rds/raw_irish_data.rds")

  tab_duplicates <- raw_irish_data %>%
    dplyr::count(Date) %>%
    dplyr::filter(n > 1)

  needed_columns <- c(
    "Date",
    "TotalConfirmedCovidCases",
    "TotalCovidDeaths",
    "HospitalisedCovidCases",
    "RequiringICUCovidCases"
  )

  missing_columns <- needed_columns[
    !needed_columns %in% names(raw_irish_data)
  ]

  if (identical(old_irish_data, raw_irish_data)) {
    usethis::ui_done("Nothing to update in irish_data.")
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
        raw_irish_data,
        "data-raw/rds/raw_irish_data.rds",
        compress = "xz"
      )

      irish_data <- raw_irish_data %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::filter(dplyr::across(.fns = ~ !is.na(.x))) %>%
        dplyr::select(
          Date,
          TotalConfirmedCovidCases,
          TotalCovidDeaths,
          HospitalisedCovidCases,
          RequiringICUCovidCases
        )

      deploy_app <- TRUE

      usethis::use_data(irish_data, overwrite = TRUE)
    })
    usethis::ui_done("irish_data dataset updated!")
  }
}
