#' Parses .dta files from SHCS into .rds (batch)
#'
#' @param indir Folder with SHCS data
#' @param outdir Folder to save rds data
#'
#' @return dta files in outdir folder
#' @export
#'
#' @examples
#'
#' \dontrun{
#' shcs_to_rds(indir = "shcs_data", outdir = "processed")
#' }


shcs_to_rds <- function(indir, outdir = "processed") {
  # Get all files in SHCS folder
  files <- list.files(path = indir)
  files_full <- stringr::str_c(indir, "/", files)
  names <- stringr::str_remove(files, ".dta")

  # Read them into a list
  all <- purrr::map(files_full, haven::read_dta, encoding = "latin1")

  # Set names to dta names
  names(all) <-  names

  # Zipp off all attributes using own function
  all <- purrr::map(all, bernr::clean_dta)

  purrr::walk2(all, names, ~readr::write_rds(.x, stringr::str_c(outdir, "/", .y, ".rds")))
}
