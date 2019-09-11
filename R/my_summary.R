#' Quick summaries command
#'
#' Wrapper around the summarise command, returns
#' n = number of observations, min = minimum value, max = maximum value,
#' mean = arithmetric mean, median = median, p25 = 25-percentile,
#' p75 = 75-percentile.
#'
#' @param df Dataframe.
#' @param col Column to summarise.
#' @param probs Probabilities for IQR, default are 0.25 to 0.75.
#'
#' @return Summarized dataframe.
#' @export
#'
#'
my_summary <- function(df, col, na.rm = TRUE) {
  probs <-  c(0.25, 0.75)
  col <- dplyr::enquo(col)
  df %>%
    dplyr::summarise(n = n(),
              min = min(!!col, na.rm = na.rm),
              max = max(!!col, na.rm = na.rm),
              mean = mean(!!col, na.rm = na.rm),
              median = median(!!col, na.rm = na.rm),
              p25 = quantile(!!col, probs = probs[1], na.rm = na.rm),
              p75 = quantile(!!col, probs = probs[2], na.rm = na.rm)) %>%
    setNames(., stringr::str_c(names(.),"_", rlang::as_name(col)))
}

