#' Get the location group id of a n2kModel object
#' @param x the n2kModel object
#' @return the location group id of the object
#' @name get_location_group_id
#' @rdname get_location_group_id
#' @exportMethod get_location_group_id
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_location_group_id",
  def = function(x) {
    standardGeneric("get_location_group_id") # nocov
  }
)

#' @rdname get_location_group_id
#' @aliases get_location_group_id,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_location_group_id",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$location_group_id)
  }
)
