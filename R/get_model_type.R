#' Get the model type of an `n2kModel` object
#' @param x the `n2kModel` object
#' @return the model type of the object
#' @name get_model_type
#' @rdname get_model_type
#' @exportMethod get_model_type
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model_type",
  def = function(x) {
    standardGeneric("get_model_type") # nocov
  }
)

#' @rdname get_model_type
#' @aliases get_model_type,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_model_type",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$model_type)
  }
)
