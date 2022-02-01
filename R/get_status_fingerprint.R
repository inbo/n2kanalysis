#' Get the status fingerprint of a n2kModel object
#' @param x the n2kModel object
#' @return the status fingerprint of the object
#' @name get_status_fingerprint
#' @rdname get_status_fingerprint
#' @exportMethod get_status_fingerprint
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_status_fingerprint",
  def = function(x) {
    standardGeneric("get_status_fingerprint") # nocov
  }
)

#' @rdname get_status_fingerprint
#' @aliases get_status_fingerprint,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_status_fingerprint",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$status_fingerprint)
  }
)
