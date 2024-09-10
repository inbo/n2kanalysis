#' Create an `Spde` object
#' @param coordinates a `data.frame` of coordinates use to define the mesh.
#' @param range a numeric vector of length 2.
#' Will be used as the `prior.range` argument of [INLA::inla.spde2.pcmatern].
#' @param Sigma a numeric vector of length 2.
#' Will be used as the `prior.sigma` argument of [INLA::inla.spde2.pcmatern].
#' @export
#' @importFrom assertthat assert_that noNA
#' @importFrom methods new
spde <- function(coordinates, range, sigma) {
  assert_that(
    inherits(coordinates, "data.frame"), is.numeric(range), is.numeric(sigma),
    noNA(coordinates), noNA(range), noNA(sigma), ncol(coordinates) == 2,
    length(range) == 2, length(sigma) == 2, all(range > 0), all(sigma > 0),
    range[2] < 1, sigma[2] < 1
  )
  new("Spde", Coordinates = coordinates, Range = range, Sigma = sigma)
}
