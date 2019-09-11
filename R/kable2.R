#' Knit table and round decimal places
#'
#' Wrapper function to quickly knit a table and round numeric values.
#'
#' @param df Dataframe you want to knit.
#' @param digits Number of decimal places, trailing zeros removed.
#'
#' @return Markdown table
#' @export
#'
#' @examples
#' kable2(mtcars)
#'
kable2 <- function(df, digits = 2) {
  knitr::kable(dplyr::mutate_if(df, is.numeric, round, digits))
}
