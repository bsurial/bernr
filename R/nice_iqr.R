#' Calculates and Interquartile ranges and prints them nicely
#'
#' This function is especially useful when used in Rmarkdown documents
#'
#' @param x Vector to calculate IQR from
#' @param to Whether values should be separated by "to", alternative is "-"
#' @param ... Arguments passed to bernr::comma()
#'
#' @return A character object
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' x <- rnorm(10)
#' nice_iqr(x)
#' # "-0.5 to 0.6"
#'
nice_iqr <- function(x, to = TRUE, ...) {

  q25 <- quantile(x)[[2]]
  q75 <- quantile(x)[[4]]

  if(to == TRUE){
    glue::glue("{bernr::comma(q25, ...)} to {bernr::comma(q75, ...)}")
  } else if(to == FALSE) {
    glue::glue("{bernr::comma(q25, ...)} - {bernr::comma(q75, ...)}")
  }
}

