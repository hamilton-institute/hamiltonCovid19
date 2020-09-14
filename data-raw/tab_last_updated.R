## code to prepare `tab_last_updated` dataset goes here

# LF: TEMP

tab_last_updated <- readr::read_csv("data-raw/csv/last_updated.csv")

usethis::use_data(tab_last_updated, overwrite = TRUE)
