#' Prints number of observations and its percantage in brackets
#'
#' This function is especially useful when used in Rmarkdown documents or tables.
#'
#' @param x Numerator, will also be displayed as n (never prints with trailing zeros.)
#' @param total Denominator
#' @param digits Digits to display after decimal point for percentage only
#' @param ... additional arguments passed to bernr::comma()
#' @param brackets Whether percentages should be displayed in bracktes, alternative is seperation with ","
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

n_pct <- function(x, total, digits = 1, brackets = TRUE, ...) {
  if(brackets == TRUE) {
    glue::glue("{bernr::comma(x, trailing = FALSE)} ({bernr::comma(x/total * 100, ...)}%)")
  } else if(brackets == FALSE) {
    glue::glue("{bernr::comma(x, trailing = FALSE)}, {bernr::comma(x/total * 100, ...)}%")
  }
}


