#' Get the file fingerprint of a n2kModel object
#' @param x the n2kModel object
#' @return the file fingerprint of the object
#' @name get_file_fingerprint
#' @rdname get_file_fingerprint
#' @exportMethod get_file_fingerprint
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_file_fingerprint",
  def = function(x) {
    standardGeneric("get_file_fingerprint") # nocov
  }
)

#' @rdname get_file_fingerprint
#' @aliases get_file_fingerprint,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_file_fingerprint",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$FileFingerprint)
  }
)

#' @rdname get_file_fingerprint
#' @aliases get_file_fingerprint,n2kManifest-methods
#' @importFrom methods setMethod new
#' @include n2k_manifest_class.R
setMethod(
  f = "get_file_fingerprint",
  signature = signature(x = "n2kManifest"),
  definition = function(x) {
    return(x@Fingerprint)
  }
)
