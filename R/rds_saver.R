#' Save rds files into folder
#'
#' This is a convenience wrapper to write files into folders and append a prefix.
#'
#' @param df Dataframe to save.
#' @param name Name of the file.
#' @param prefix Prefix before file, default = 00.
#' @param postfix Postfix after file, default = None.
#' @param folder Folder relative to project root.
#'
#' @export
#'
#' @examples
#' rds_saver(mtcars, name = "mtcars", folder = "processed")
#'
rds_saver <- function(df, name, prefix = "00", postfix = "", folder = "processed") {
  readr::write_rds(df,
                   path = here::here(folder, stringr::
                                       str_c(prefix, "-", name, postfix, ".rds")))
}


