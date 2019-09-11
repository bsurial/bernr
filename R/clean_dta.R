#' Clean stata dataframes
#'
#' This function deletes all attributes from dataframes, especially those imported
#' from stata. Additionally, it converts empty character cells to NA.
#'
#' @param df The dataframe, typically a file imported from stata.
#' @return A datafame without attributes and all empty character cells ("") are
#'   converted to NA.
#' @examples
#' # needs sjlabelled and haven packages installed
#' clean_dta(mtcars)


clean_dta <- function(df) {
  df %>%
    sjlabelled::remove_label() %>% # remove labels
    haven::zap_formats() %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "") # "" to NA in character columns
}
