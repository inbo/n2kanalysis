#' The `spde` class
#'
#' It holds the coordinates, range and sigma parameters for the SPDE model.
#' @slot Coordinates a `data.frame` with the coordinates used for the mesh.
#' @slot Range a numeric vector of length 2.
#' Will be used as the `prior.range` argument of [INLA::inla.spde2.pcmatern].
#' @slot Sigma a numeric vector of length 2.
#' Will be used as the `prior.sigma` argument of [INLA::inla.spde2.pcmatern].
#' @name Spde-class
#' @rdname Spde-class
#' @exportClass Spde
#' @aliases Spde-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "Spde",
  representation = representation(
    Coordinates = "data.frame", Range = "numeric", Sigma = "numeric"
  )
)

#' @importFrom assertthat assert_that noNA
#' @importFrom methods setValidity
setValidity(
  "Spde",
  function(object) {
    assert_that(
      noNA(object@Coordinates), noNA(object@Range), noNA(object@Sigma),
      ncol(object@Coordinates) == 2, length(object@Range) == 2,
      length(object@Sigma) == 2, all(object@Range > 0), all(object@Sigma > 0),
      object@Range[2] < 1, object@Sigma[2] < 1
    )
    return(TRUE)
  }
)
