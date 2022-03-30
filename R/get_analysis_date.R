#' Get the analysis date of an `n2kModel` object
#' @param x the `n2kModel` object
#' @return the analysis date of the object
#' @name get_analysis_date
#' @rdname get_analysis_date
#' @exportMethod get_analysis_date
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_analysis_date",
  def = function(x) {
    standardGeneric("get_analysis_date") # nocov
  }
)

#' @rdname get_analysis_date
#' @aliases get_analysis_date,n2kModel-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_analysis_date",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$analysis_date)
  }
)
