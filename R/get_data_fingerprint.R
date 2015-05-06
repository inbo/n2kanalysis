#' Get the data fingerprint of a n2kModel object
#' @param x the n2kModel object
#' @return the data fingerprint of the object
#' @name get_data_fingerprint
#' @rdname get_data_fingerprint
#' @exportMethod get_data_fingerprint
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_data_fingerprint", 
  def = function(x){
    standard.generic("get_data_fingerprint")
  }
)

#' @rdname get_data_fingerprint
#' @aliases get_data_fingerprint,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_data_fingerprint",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@DataFingerprint)
  }
)
