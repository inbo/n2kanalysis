#' Aggregate all results meta data in a single dataframe
#' @param x object with the current results
#' @param ... further arguments (see Details)
#' @name result_metadata
#' @rdname result_metadata
#' @exportMethod result_metadata
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "result_metadata",
  def = function(x, ...) {
    standardGeneric("result_metadata") # nocov
  }
)
