#' Calculate the mean and 95\% confidence interval from the inverse of a
#' marginal
#'
#' This is useful for calculating the variance of a random effect when the
#' marginal gives the precision of the random effect.
#' @param marginal The INLA marginal
#' @importFrom INLA inla.tmarginal inla.emarginal inla.qmarginal
#' @export
inla_inverse <- function(marginal) {
  inverse <- inla.tmarginal(
    fun = function(x) {
      1 / x
    },
    marginal
  )
  tibble(
    Estimate = inla.emarginal(
      function(x) {
        x
      },
      inverse
    ),
    LowerConfidenceLimit = inla.qmarginal(0.025, inverse),
    UpperConfidenceLimit = inla.qmarginal(0.975, inverse)
  )
}
