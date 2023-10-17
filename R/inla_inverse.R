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
  inverse <- INLA::inla.tmarginal(
    marginal,
    fun = function(x) {
      1 / x
    }
  )
  estimate <- try(
    INLA::inla.emarginal(
      inverse,
      fun = function(x) {
        x
      }
    )
  )
  if (inherits(estimate, "try-error")) {
    return(
      data.frame(
        estimate = NA, lower_confidence_limit = NA, upper_confidence_limit = NA
      )
    )
  }
  tibble(
    estimate = estimate,
    lower_confidence_limit = INLA::inla.qmarginal(0.025, inverse),
    upper_confidence_limit = INLA::inla.qmarginal(0.975, inverse)
  )
}
