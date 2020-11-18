## Update irish_icu_data dataset

usethis::ui_todo("Checking update for irish_icu_data dataset...")

`%>%` <- magrittr::`%>%`

url_icu <- "https://services-eu1.arcgis.com/z6bHNio59iTqqSUY/arcgis/rest/services/ICUBISHistoricTimelinePublicView/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json"

res <- tryCatch(
  httr::GET(url_icu),
  error = function(x) NULL
)

raw_irish_icu_data <- res %>%
  httr::content() %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("features", "attributes") %>%
  dplyr::rename(Date = extract)

###

if (is.null(raw_irish_icu_data)) {
  usethis::ui_oops(paste0("Failed to connect to ", url_irl))
} else {
  # Testing new dataset
  old_irish_icu_data <- readr::read_rds("data-raw/rds/raw_irish_icu_data.rds")

  tab_duplicates <- raw_irish_icu_data %>%
    dplyr::count(Date) %>%
    dplyr::filter(n > 1)

  needed_columns <- c(
    "Date",
    "ncovidconf",
    "ndischcovidconf",
    "adcconf"
  )

  missing_columns <- needed_columns[
    !needed_columns %in% names(raw_irish_icu_data)
  ]

  if (identical(old_irish_icu_data, raw_irish_icu_data) |
      nrow(old_irish_icu_data) > nrow(raw_irish_icu_data)) {
    usethis::ui_done("Nothing to update in irish_icu_data")
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
        raw_irish_icu_data,
        "data-raw/rds/raw_irish_icu_data.rds",
        compress = "xz"
      )

      irish_icu_data <- raw_irish_icu_data %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
          DateTime = lubridate::as_datetime(Date/1000),
          DateTime = DateTime - lubridate::hours(3), # fixing time difference
          Date = lubridate::as_date(DateTime)
        ) %>%
        dplyr::select(
          Date,
          DateTime,
          currentICUCases = ncovidconf,
          last24ICUAdmissions = adcconf,
          last24ICUDischarges = ndischcovidconf
        )

      deploy_app <- TRUE

      usethis::use_data(irish_icu_data, overwrite = TRUE)
    })

    usethis::ui_done("irish_icu_data dataset updated!")
  }
}
