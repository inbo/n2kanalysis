#' Get the data of a n2kModel object
#' @param x the n2kModel object
#' @return the data of the object
#' @name get_data
#' @rdname get_data
#' @exportMethod get_data
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_data",
  def = function(x){
    standardGeneric("get_data") # nocov
  }
)

#' @rdname get_data
#' @aliases get_data,n2kGlmerPoisson-methods
#' @importFrom methods setMethod new
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "get_data",
  signature = signature(x = "n2kGlmerPoisson"),
  definition = function(x){
    return(x@Data)
  }
)

#' @rdname get_data
#' @aliases get_data,n2kInlaNbinomial-methods
#' @importFrom methods setMethod new
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "get_data",
  signature = signature(x = "n2kInlaNbinomial"),
  definition = function(x){
    return(x@Data)
  }
)
