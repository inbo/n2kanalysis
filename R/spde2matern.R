#' Convert an `Spde` object to a matern object
#' @param object The `Spde` object
#' @name spde2matern
#' @rdname spde2matern
#' @exportMethod spde2matern
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "spde2matern",
  def = function(object) {
    standardGeneric("spde2matern") # nocov
  }
)

#' @rdname spde2mesh
#' @importFrom methods setMethod new
#' @include spde_class.R
setMethod(
  f = "spde2matern",
  signature = signature(object = "Spde"),
  definition = function(object) {
    stopifnot(
      "INLA package required but not installed." =
        requireNamespace("INLA", quietly = TRUE)
    )
    INLA::inla.spde2.pcmatern(
      mesh = spde2mesh(object), prior.range = object@Range,
      prior.sigma = object@Sigma
    )
  }
)
