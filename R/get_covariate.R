#' Get the covariate of a n2kModel object
#' @param x the n2kModel object
#' @return the covariate of the object
#' @name get_covariate
#' @rdname get_covariate
#' @exportMethod get_covariate
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_covariate", 
  def = function(x){
    standard.generic("get_covariate")
  }
)

#' @rdname get_covariate
#' @aliases get_covariate,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_covariate",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@Covariate)
  }
)
