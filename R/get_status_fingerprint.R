#' Get the status fingerprint of a n2kModel object
#' @param x the n2kModel object
#' @return the status fingerprint of the object
#' @name get_status_fingerprint
#' @rdname get_status_fingerprint
#' @exportMethod get_status_fingerprint
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_status_fingerprint", 
  def = function(x){
    standard.generic("get_status_fingerprint")
  }
)

#' @rdname get_status_fingerprint
#' @aliases get_status_fingerprint,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_status_fingerprint",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@StatusFingerprint)
  }
)
