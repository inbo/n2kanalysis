#' The n2kResult class
#' @name n2kResult-class
#' @rdname n2kResult-class
#' @exportClass n2kResult
#' @aliases n2kResult-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kAnalysisVersion_class.R
#' @include n2kAnalysisMetadata_class.R
#' @include n2kParameter_class.R
setClass(
  "n2kResult",
  contains = c("n2kAnalysisMetadata", "n2kAnomaly")
)

#' @importFrom methods setValidity
setValidity(
  "n2kResult",
  function(object){
    if (!all(
      object@AnalysisMetadata$AnalysisVersion %in%
        object@AnalysisVersion$Fingerprint
    )) {
      stop(
"Some AnalysisVersion in 'AnalysisMetadata' slot are not present in
'AnalysisVersion' slot"
      )
    }
    if (!all(
object@ParameterEstimate$Analysis %in% object@AnalysisMetadata$FileFingerprint
    )) {
      stop(
"Some Analysis in 'ParameterEstimate' slot are not present in 'AnalysisMetadata'
slot"
      )
    }
    if (!all(
      object@Anomaly$Analysis %in% object@AnalysisMetadata$FileFingerprint
    )) {
      stop(
"Some Analysis in 'Anomaly' slot are not present in 'AnalysisMetadata' slot"
      )
    }
    return(TRUE)
  }
)
