#' Get the seed of an `n2kModel` object
#' @param x the `n2kModel` object
#' @return the seed of the object
#' @name get_seed
#' @rdname get_seed
#' @exportMethod get_seed
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_seed",
  def = function(x) {
    standardGeneric("get_seed") # nocov
  }
)

#' @rdname get_seed
#' @aliases get_seed,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_seed",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$seed)
  }
)
