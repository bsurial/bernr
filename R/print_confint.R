#' Creates nice confidence report for tables
#'
#' Creates a nice and convenient output for tables and graphs to report estimates
#' with their corresponding confidence intervals, e.g. 1.2 (95% CI 1.1 - 1.3).
#'
#' @param df Dataframe.
#' @param new_col Name of the new column to create.
#' @param estimate Estimate for which confidence interval was created.
#' @param lcl Lower confidence value.
#' @param ucl Upper confidence value.
#' @param digits Round to digits.
#' @param to Separator used in the brackets.
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' mtcars %>%
#' summarise(mean = mean(mpg),
#'           n = n(),
#'           se = sd(mpg)/sqrt(n),
#'           lcl = mean - 1.96 * se,
#'           ucl = mean + 1.96 * se) %>%
#'           print_confint("mean_ci", estimate = mean, lcl = lcl,
#'                       ucl = ucl, digits = 2, to = " to ")
print_confint <- function(df, new_col, estimate, lcl, ucl, digits = 1, to = " - ") {
  estimate <- dplyr::enquo(estimate)
  lcl <- dplyr::enquo(lcl)
  ucl <- dplyr::enquo(ucl)
  df %>%
    dplyr::mutate(new_col := paste0(round(!!estimate, digits), " (95% CI ",
                                    round(!!lcl, digits), to,
                                    round(!!ucl, digits), ")"))
}
