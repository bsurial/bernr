#' Count individual IDs
#'
#' Counts the number of unique ids within a dataframe
#'
#' @param df Dataframe.
#' @param id Column which is the id, default = id.
#'
#' @return Number of individual ids
#' @export
#'
#' @examples
#' mtcars$id <- 1:32
#' distinct_ids(mtcars)
#' #32


distinct_ids <- function(df, id = id) {
  df %>%
    dplyr::summarise(distinct_ids = dplyr::across({{ id }}, dplyr::n_distinct)) %>%
    dplyr::pull(distinct_ids)
}

