#' Get the parent status of an `n2kModel`
#' @param x the `n2kModel` object
#' @return the parent status of the object
#' @name parent_status
#' @rdname parent_status
#' @exportMethod parent_status
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "parent_status",
  def = function(x) {
    standardGeneric("parent_status") # nocov
  }
)

#' @rdname parent_status
#' @aliases parent_status,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "parent_status",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisRelation)
  }
)

#' Overwrite the status of an `n2kAnalysisMetadata`
#' @param x the `n2kAnalysisMetadata` object
#' @param value the new values for the status
#' @name parent_status<-
#' @rdname parent.status.change
#' @exportMethod parent_status<-
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "parent_status<-",
  def = function(x, value) {
    standardGeneric("parent_status<-") # nocov
  }
)

#' @rdname parent.status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_composite_class.R
setReplaceMethod(
  "parent_status",
  "n2kComposite",
  function(x, value) {
    x@parent_status <- value
    x@status_fingerprint <- sha1(
      list(
        x@AnalysisMetadata$file_fingerprint, x@AnalysisMetadata$status,
        x@Parameter, x@Index, x@AnalysisMetadata$analysis_version,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)
