## Update interventions_data dataset

`%>%` <- magrittr::`%>%`

url <- "https://data.humdata.org/dataset/e1a91ae0-292d-4434-bc75-bf863d4608ba/resource/720d664f-e6c2-4dfb-848f-e3ce92838785/download/acaps_covid19_government_measures_dataset.xlsx"

raw_interventions_data <- tryCatch(
  {
    file_name <- tempfile("temp.xlsx")
    download.file(
      url = url,
      destfile = file_name,
      quiet = TRUE
    )
    readxl::read_excel(file_name, sheet = "Dataset")
  },
  error = function(x) NULL
)

if (is.null(raw_interventions_data)) {
  stop(paste0("Failed to connect to ", url))
}

old_interventions_data <- readr::read_rds("data-raw/rds/raw_interventions_data.rds")

if (identical(old_interventions_data, raw_interventions_data)) {
  stop("Nothing to update.")
}

readr::write_rds(
  raw_interventions_data,
  "data-raw/rds/raw_interventions_data.rds",
  compress = "xz"
)

interventions_data <- raw_interventions_data %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    country = dplyr::recode(
      country,
      'United States of America' = 'USA',
      'United Kingdom' = 'UK',
      "Czech Republic" = 'Czechia'
    ),
    measure = stringr::str_to_sentence(measure),
    measure = stringr::str_squish(measure)
  )

deploy_app <- TRUE

usethis::use_data(interventions_data, overwrite = TRUE)
