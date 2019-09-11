#' Columnwise percentage calculation within pipe
#'
#' After counting or summarising in dplyr, generates percentages instead of
#' probabilites. It is a wrapper around round(n/sum(n)*100).
#'
#' @param n Character object.
#' @param digits Number of decimal places to display.
#'
#' @return Column with percentage.
#' @export
#'
#' @examples
#' mtcars %>%
#'   count(cyl) %>%
#'   mutate(p = my_percent(n))
#'
my_percent <- function(n, digits = 1) {
  round(n/sum(n)*100, digits)
}



