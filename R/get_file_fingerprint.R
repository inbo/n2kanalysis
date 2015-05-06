#' Get the file fingerprint of a n2kModel object
#' @param x the n2kModel object
#' @return the file fingerprint of the object
#' @name get_file_fingerprint
#' @rdname get_file_fingerprint
#' @exportMethod get_file_fingerprint
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_file_fingerprint", 
  def = function(x){
    standard.generic("get_file_fingerprint")
  }
)

#' @rdname get_file_fingerprint
#' @aliases get_file_fingerprint,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_file_fingerprint",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@FileFingerprint)
  }
)
