## Update interventions_data dataset

`%>%` <- magrittr::`%>%`

base_url <- "https://data.humdata.org/dataset/e1a91ae0-292d-4434-bc75-bf863d4608ba"

raw_interventions_data <- tryCatch(
  {
    res <- httr::GET(base_url)

    endpoint <- res %>%
      xml2::read_html() %>%
      xml2::xml_find_first("//a[contains(@href, 'xlsx')]") %>%
      xml2::xml_attr("href")

    url <- paste0("https://data.humdata.org", endpoint)

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
  message(paste0("Failed to download a Excel file from ", url))
} else {
  # Testing new dataset
  old_interventions_data <-
    readr::read_rds("data-raw/rds/raw_interventions_data.rds")

  tab_duplicates <-  suppressMessages(
    janitor::get_dupes(raw_interventions_data)
  )

  needed_columns <- c(
    "COUNTRY",
    "REGION",
    "LOG_TYPE",
    "CATEGORY",
    "MEASURE",
    "COMMENTS",
    "DATE_IMPLEMENTED",
    "SOURCE",
    "SOURCE_TYPE",
    "LINK",
    "ENTRY_DATE"
  )

  missing_columns <- needed_columns[
    !needed_columns %in% names(raw_interventions_data)
  ]

  if (identical(raw_interventions_data, old_interventions_data)) {
    usethis::ui_done("Nothing to update in global_data.")
  } else if (nrow(tab_duplicates) > 0) {
    usethis::ui_oops("New data has duplicated entries. Dataset not updated.")
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
        ) %>%
        dplyr::select(
          country,
          region,
          log_type,
          category,
          measure,
          comments,
          date_implemented,
          source,
          source_type,
          link,
          entry_date
        )

      deploy_app <- TRUE

      # Saving dataset
      usethis::use_data(interventions_data, overwrite = TRUE)
    })
    usethis::ui_done("interventions_data dataset updated!")
  }
}
