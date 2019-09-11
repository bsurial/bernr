#' Confidence and prediction intervals from lme objects
#'
#' This function takes a mixed effect model (lme object) and a new dataframe as
#'   input and creates predictions, confidence intervals and prediction intervals
#'   using the method described by
#'   \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions}{Ben Bolker}.
#'
#'
#' @param model The lme-model used for prediction.
#' @param newdat The data.frame to use for prediction.
#' @param pred_int Calculate prediction intervals (logical).
#' @param conf_level Confidence level for the calculation of both intervals.
#' @return A datafame which has predictions, standarderrors, confidence and.
#'   prediction intervals
#' @examples
#' # depends on an lme object from the nlme-package
#' m <- nlme::lme(mpg ~ disp + hp, random = ~1|cyl, data = mtcars)
#'
#' # create new data.frame
#' new <- data.frame(disp = c(160, 170), hp = c(90, 100))
#' bolker_ci(m, new)

bolker_ci <- function(model, newdat, pred_int = FALSE, conf_level = 0.95) {
  if(class(model) != "lme") {
    stop("works for lme-models only")
  }
  z <- round(qnorm((1-conf_level)/2, lower.tail = FALSE), 2)
  newdat$pred <- predict(model, newdat, level = 0)
  Designmat <- model.matrix(formula(model)[-2], newdat)
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat))
  newdat$se <- sqrt(predvar)
  newdat$ci_l <- newdat$pred - z*newdat$se
  newdat$ci_h <- newdat$pred + z*newdat$se
  if(pred_int == TRUE) {
    newdat$se2 <- sqrt(predvar+model$sigma^2)
    newdat$predint_l <- newdat$pred - z*newdat$se2
    newdat$predint_h <- newdat$pred + z*newdat$se2
  }
  newdat
}



