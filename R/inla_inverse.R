#' Calculate the mean and 95\% confidence interval from the inverse of a
#' marginal
#'
#' This is useful for calculating the variance of a random effect when the
#' marginal gives the precision of the random effect.
#' @param marginal The INLA marginal
#' @export
inla_inverse <- function(marginal) {
  assert_that(
    requireNamespace("INLA", quietly = TRUE),
    msg = "INLA package required but not installed."
  )
  inverse <- INLA::inla.tmarginal(fun = function(x) { 1 / x }, marginal)
  tibble(
    estimate = INLA::inla.emarginal(function(x) { x }, inverse),
    lower_confidence_lLimit = INLA::inla.qmarginal(0.025, inverse),
    upper_confidence_limit = INLA::inla.qmarginal(0.975, inverse)
  )
}
