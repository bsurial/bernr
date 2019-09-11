#' Confidence intervals of bootMer objects
#'
#' @param boot bootMer object from which to retreive confidence interval
#'
#' @return A dataframe with confidence intervals
#' @export
#'
#' @examples
#' \dontrun{
#' boot_cis(bootMer.object)
#' }

boot_cis <- function(boot){
  boot <- boot$t %>% dplyr::as_tibble()
  cis <- t(purrr::map_df(boot, ~quantile(.x, probs = c(0.025, 0.975))))
  colnames(cis) <- c("l_ci", "u_ci")
  dplyr::as_tibble(cis)
}

