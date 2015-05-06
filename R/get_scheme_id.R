#' Get the scheme_id of a n2kModel object
#' @param x the n2kModel object
#' @return the scheme_id of the object
#' @name get_scheme_id
#' @rdname get_scheme_id
#' @exportMethod get_scheme_id
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_scheme_id", 
  def = function(x){
    standard.generic("get_scheme_id")
  }
)

#' @rdname get_scheme_id
#' @aliases get_scheme_id,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_scheme_id",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@SchemeID)
  }
)
