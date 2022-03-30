#' Get the `scheme_id` of an `n2kModel` object
#' @param x the `n2kModel` object
#' @return the `scheme_id` of the object
#' @name get_scheme_id
#' @rdname get_scheme_id
#' @exportMethod get_scheme_id
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_scheme_id",
  def = function(x) {
    standardGeneric("get_scheme_id") # nocov
  }
)

#' @rdname get_scheme_id
#' @aliases get_scheme_id,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_scheme_id",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$scheme_id)
  }
)
