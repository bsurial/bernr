#' Knit tableone with nonnormal values
#'
#' @param x Tableone object
#' @param nonnormal Vector containing nonnormal values for which median
#' is wanted.
#' @param ... Further arguments passed to knitr::kable()
#'
#' @return Knitted version of a tableone object.
#' @export
#'
#' @examples
#' library(tableone)
#' vars <- c("mpg", "cyl", "disp", "hp", "drat", "wt")
#' nonnormal <- c("mpg", "disp", "hp", "drat")
#' t1 <- tableone::CreateTableOne(vars, data = mtcars)
#' kableone_nonnorm(t1, nonnormal = nonnormal)
#'
kableone_nonnorm <- function (x, nonnormal, ...)
{
  capture.output(x <- print(x, nonnormal = nonnormal))
  knitr::kable(x, ...)
}
