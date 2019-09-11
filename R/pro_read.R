#' Read rds files from 'processed' folder
#'
#' Reads rds files directly from the processed folder, which should be in
#'   the project's root folder.
#'
#' @param file Name of the file.
#' @examples
#' pro_read("dataframe.rds")

# Read from processed folder
pro_read <- function(file) {
  readr::read_rds(here::here("processed", file))
}
