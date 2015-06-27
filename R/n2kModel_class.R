#' The n2kModel class
#' 
#' A virtual superclass to contain the analysis models
#' @name n2kModel-class
#' @rdname n2kModel-class
#' @exportClass n2kModel
#' @aliases n2kModel-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kAnalysisMetadata_class.R
setClass(
  "n2kModel",
  representation = representation(
    "VIRTUAL"
  ),
  contains = "n2kAnalysisMetadata"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character check_single_posix
setValidity(
  "n2kModel",
  function(object){
    if (nrow(object@AnalysisMetadata) != 1) {
      stop("The 'AnalysisMetadata' slot must contain exactly one row")
    }
    if (nrow(object@AnalysisRelation) > 0) {
      if (any(object@AnalysisRelation$Analysis != object@AnalysisMetadata$FileFingerprint)) {
        stop("Some Analysis in 'AnalysisRelation' slot don't match FileFingerprint")
      }
    }
    return(TRUE)
  }
)
