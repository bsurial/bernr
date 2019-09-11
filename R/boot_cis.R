#' Extract 95% CIs from bootMer objects
#'
#' @param boot Boot created with the bootMer command in the lme4 package.
#'
#' @return Dataframe with upper and lower confidence intervals.
#' @export
#'
boot_cis <- function(boot){
  boot <- boot$t %>% dplyr::as_tibble()
  cis <- t(purr::map_df(boot, ~quantile(.x, probs = c(0.025, 0.975))))
  colnames(cis) <- c("l_ci", "u_ci")
  dplyr::as_tibble(cis)
}

