#' Calculate Median (Interquartile range) and return character.
#'
#' Calculates the median and interquartile range of a numeric column in a dataframe
#' and returns a character column in the form of median (Q25 - Q75).
#'
#' @param df Dataframe.
#' @param new_col Name of the new column.
#' @param variable Numeric column used for calculation.
#' @param digits Optional number of decimal places.
#' @param to Optional character placed between Q25 and Q75, default is " - ".
#'
#' @return Summarised character column.
#' @export
#'
#' @examples
#' median_iqr2(mtcars, "mpg", mpg)
#' median_iqr2(mtcars, "mpg", mpg, to = " to ")

median_iqr2 <- function(df, new_col, variable, digits = 1, to = " - ") {
  variable = dplyr::enquo(variable)
  df %>%
    dplyr::summarise(median = median(!!variable),
              q25 = quantile(!!variable, 0.25),
              q75 = quantile(!!variable, 0.75)) %>%
    dplyr::mutate_if(is.numeric, round, digits) %>%
    dplyr::mutate(!! new_col := paste0(median, " (IQR ", q25, to , q75, ")")) %>%
    dplyr::select(-c(median, q25, q75))
}


