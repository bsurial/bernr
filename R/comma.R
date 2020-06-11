#' Formats rounded characters nicely for print purposes
#'
#' This function is useful if used in Rmarkdown documents or when used to print nice tables.
#'
#' @param x A numeric object to modify
#' @param digits Desired number of digits after the decimal point
#' @param trailing Should trailing zeroes be preserved? Default = TRUE
#'
#' @return A properly formatted character object
#' @export
#'
#' @examples
#'
#' x <- 200.00
#' comma(x, digits = 1)
#' # "200.0"
#' comma(x, digits = 1, trailing = FALSE)
#' # "200"

comma <- function(x, digits = 1, trailing = TRUE) {
  if(trailing == TRUE) {
  formatC(round(x, digits = digits), format = "f",
          digits = digits, big.mark = "'")
  } else if(trailing == FALSE) {
  format(round(x, digits = digits), big.mark = "'")
  }
}


