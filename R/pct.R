#' Calculates and nicely prints percent values based on two arguments
#'
#' This function is especially useful when used in Rmarkdown documents.
#'
#' @param x Numerator
#' @param total Denominator
#' @param digits Digits to display after decimal point
#' @param trailing Whether trailing zeroes should be displayed
#'
#' @return A character object
#' @export
#'
#' @examples
#'
#' pct(10, 100)
#' #"10.0%"
#' pct(10, 100, trailing = FALSE)
#' #"10%"

pct <- function(x, total, digits = 1, trailing = TRUE) {
  if(trailing == TRUE) {
    paste0(formatC(round(x / total * 100, digits), format = "f",
                   digits = digits, big.mark = "'"), "%")
    } else if(trailing == FALSE) {
    paste0(round(x / total * 100, digits), "%")
  }
}


