#' Round function for printing
#'
#' This function can be used for tables, e.g. when trailing zeros are wanted.
#'
#' @param x Numeric object.
#' @param digits Number of digits after comma, default = 1.
#'
#' @return Rounded character object with trailing zeros.
#' @export
#'
#' @examples
#'
#' mround(10.123, digits = 1)
#' # "10.1"
#' mround(10.000, digits = 1)
#' # "10.0"
#'
mround <- function(x, digits = 1) {
  formatC(round(x, digits), format = "f", digits = digits)
}

