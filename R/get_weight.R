#' Get the weight of a n2kModel object
#' @param x the n2kModel object
#' @return the weight of the object
#' @name get_weight
#' @rdname get_weight
#' @exportMethod get_weight
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_weight", 
  def = function(x){
    standard.generic(get_weight)
  }
)

#' @rdname get_weight
#' @aliases get_weight,n2kGlmerPoisson-methods
#' @importFrom methods setMethod
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "get_weight",
  signature = signature(x = "n2kGlmerPoisson"),
  definition = function(x){
    return(x@Weight)
  }
)
