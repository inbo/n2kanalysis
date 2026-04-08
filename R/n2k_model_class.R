#' The `n2kModel` class
#'
#' A virtual superclass to contain the analysis models
#' @name n2kModel-class
#' @rdname n2kModel-class
#' @exportClass n2kModel
#' @aliases n2kModel-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_analysis_metadata_class.R
setClass(
  "n2kModel",
  representation = representation(
    "VIRTUAL"
  ),
  contains = "n2kAnalysisMetadata"
)

#' @importFrom methods setValidity
setValidity(
  "n2kModel",
  function(object) {
    if (nrow(object@AnalysisMetadata) != 1) {
      stop("The 'AnalysisMetadata' slot must contain exactly one row")
    }
    # fmt: skip
    stopifnot(
      "Some Analysis in 'AnalysisRelation' slot don't match file_fingerprint" =
        nrow(object@AnalysisRelation) == 0 ||
        all(
          object@AnalysisRelation$analysis ==
            object@AnalysisMetadata$file_fingerprint
        )
    )
    return(TRUE)
  }
)
