#' Create nice P-values
#'
#' This function returns nice p-values. This is useful especially for us with rmarkdown files and knitr::tables.
#'
#' @param pval P-value to be converted
#' @param digits Number of digits to round
#'
#' @return A numeric object
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100, mean = 0)
#' y <- rnorm(100, mean = 10)
#'
#' t <- t.test(x, y)
#' nice_p(t$p.value)
#'
nice_p <- function(pval, digits = 4) {
  if(!is.numeric(pval)) {
    stop("pval has tu be numeric")
  }
  ifelse(pval < 0.001, "<0.001", format(round(pval, digits), scientific = FALSE))
}
