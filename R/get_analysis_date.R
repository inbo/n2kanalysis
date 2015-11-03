#' Get the analysis date of a n2kModel object
#' @param x the n2kModel object
#' @return the analysis date of the object
#' @name get_analysis_date
#' @rdname get_analysis_date
#' @exportMethod get_analysis_date
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_analysis_date",
  def = function(x){
    standard.generic("get_analysis_date") # nocov
  }
)

#' @rdname get_analysis_date
#' @aliases get_analysis_date,n2kModel-methods
#' @importFrom methods setMethod
#' @include n2kAnalysisMetadata_class.R
setMethod(
  f = "get_analysis_date",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x){
    return(x@AnalysisMetadata$AnalysisDate)
  }
)
