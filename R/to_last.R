#' Move column to the end of dataframe
#'
#' @param df Dataframe
#' @param ... Columns that should be at the end of the Dataframe.
#'
#' @export
#'
#' @examples
#' to_last(mtcars, cyl)
to_last <- function(df, ...) {
  cols <- dplyr::enquos(...)

  df %>%
    dplyr::select(-c(!!!cols), c(!!!cols))
}

to_last(mtcars, cyl)
