#' Add the results from an analysis
#' @param x object with the current results
#' @param ... further arguments (see Details)
#' @name get_result
#' @rdname get_result
#' @exportMethod get_result
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_result",
  def = function(x, ...) {
    standardGeneric("get_result") # nocov
  }
)
