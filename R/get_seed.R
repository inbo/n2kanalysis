#' Get the seed of a n2kModel object
#' @param x the n2kModel object
#' @return the seed of the object
#' @name get_seed
#' @rdname get_seed
#' @exportMethod get_seed
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_seed", 
  def = function(x){
    standard.generic("get_seed")
  }
)

#' @rdname get_seed
#' @aliases get_seed,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_seed",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@Seed)
  }
)
