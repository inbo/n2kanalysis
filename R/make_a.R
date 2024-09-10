#' Convert an `Spde` object to a A object
#' @param object The `Spde` object
#' @name make_a
#' @rdname make_a
#' @exportMethod make_a
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "make_a",
  def = function(object, data) {
    standardGeneric("make_a") # nocov
  }
)

#' @rdname make_a
#' @importFrom methods setMethod new
#' @include spde_class.R
setMethod(
  f = "make_a",
  signature = signature(object = "Spde"),
  definition = function(object, data) {
    assert_that(
      inherits(data, "data.frame"),
      all(colnames(object@Coordinates) %in% colnames(data))
    )
    stopifnot(
      "INLA package required but not installed." =
        requireNamespace("INLA", quietly = TRUE)
    )
    data[colnames(object@Coordinates)] |>
      as.matrix() |>
      INLA::inla.spde.make.A(mesh = spde2mesh(object))
  }
)
