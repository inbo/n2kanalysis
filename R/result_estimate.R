#' Aggregate all results estimates in a single dataframe
#' @param x object with the current results
#' @param ... further arguments (see Details)
#' @name result_estimate
#' @rdname result_estimate
#' @exportMethod result_estimate
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "result_estimate",
  def = function(x, ...) {
    standardGeneric("result_estimate") # nocov
  }
)
