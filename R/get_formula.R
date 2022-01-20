#' Get the formula of a n2kModel object
#' @param x the n2kModel object
#' @return the formula of the object
#' @name get_formula
#' @rdname get_formula
#' @exportMethod get_formula
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_formula",
  def = function(x) {
    standardGeneric("get_formula") # nocov
  }
)

#' @rdname get_formula
#' @aliases get_formula,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_formula",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisFormula)
  }
)
