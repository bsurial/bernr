#' Calculate Median (Interquartile range) and return numeric.
#'
#' Calculates the median and interquartile range of a numeric column in a dataframe
#' and returns three numeric columns.
#'
#' @param df Dataframe.
#' @param variable Numeric column used for calculation.
#'
#' @return Three new columns with median, q25, q75.
#' @export
#'
#' @examples
#' median_iqr(mtcars, mpg)
#' #>   median    q25  q75
#' #> 1   19.2 15.425 22.8
median_iqr <- function(df, variable) {
  variable = dplyr::enquo(variable)
  df %>%
    dplyr::summarise(median = median(!!variable),
              q25 = quantile(!!variable, 0.25),
              q75 = quantile(!!variable, 0.75))
}

