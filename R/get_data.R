#' Get the data of an `n2kModel` object
#' @param x the `n2kModel` object
#' @return the data of the object
#' @name get_data
#' @rdname get_data
#' @exportMethod get_data
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_data",
  def = function(x) {
    standardGeneric("get_data") # nocov
  }
)

#' @rdname get_data
#' @aliases get_data,n2kInla-methods
#' @importFrom methods setMethod new
#' @include n2k_inla_class.R
setMethod(
  f = "get_data",
  signature = signature(x = "n2kInla"),
  definition = function(x) {
    return(x@Data)
  }
)
