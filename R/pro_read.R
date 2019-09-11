#' Read rds files from 'processed' folder
#'
#' Reads rds files directly from the processed folder, which should be in
#'   the project's root folder.
#'
#' @param file Name of the file.
#'@examples
#'\dontrun{
#'pro_read("file.rds")
#'}

# Read from processed folder
pro_read <- function(file) {
  readr::read_rds(here::here("processed", file))
}
