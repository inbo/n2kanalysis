#' Get the model type of a n2kModel object
#' @param x the n2kModel object
#' @return the model type of the object
#' @name get_model_type
#' @rdname get_model_type
#' @exportMethod get_model_type
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model_type",
  def = function(x){
    standard.generic("get_model_type")
  }
)

#' @rdname get_model_type
#' @aliases get_model_type,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod
#' @include n2kAnalysisMetadata_class.R
setMethod(
  f = "get_model_type",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x){
    return(x@AnalysisMetadata$ModelType)
  }
)
