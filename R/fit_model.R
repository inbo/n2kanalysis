#' Fit a n2kModel object
#' @param x the n2kModel
#' @param ... other arguments. See details
#' @name fit_model
#' @rdname fit_model
#' @exportMethod fit_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "fit_model",
  def = function(x, ...) {
    standardGeneric("fit_model") # nocov
  }
)
