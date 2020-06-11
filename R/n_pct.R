#' Prints number of observations and its percantage in brackets
#'
#' This function is especially useful when used in Rmarkdown documents or tables.
#'
#' @param x Numerator
#' @param total Denominator
#' @param digits Digits to display after decimal point
#' @param brackets Whether percentages should be displayed in bracktes, alternative is seperation with ","
#' @param trailing Whether trailing zeroes should be displayed
#'
#' @return
#' @export
#'
#' @examples
#' n_pct(10, 1000)
#' # 10.0 (1.0%)
#'
#' n_pct(10, 1000, trailing = TRUE, brackets = FALSE)
#' # 10.0, 1.0%
#'
#' n_pct(10, 1000, trailing = FALSE, brackets = FALSE)
#' # 10, 1%
#'

n_pct <- function(x, total, digits = 1, brackets = TRUE, trailing = TRUE) {
  if(brackets == TRUE) {
    glue::glue("{bernr::comma(x, trailing = trailing)} ({bernr::comma(x/total * 100, digits = digits, trailing = trailing)}%)")
  } else if(brackets == FALSE) {
    glue::glue("{bernr::comma(x, trailing = trailing)}, {bernr::comma(round(x/total * 100), digits = digits, trailing = trailing)}%")
  }
}


