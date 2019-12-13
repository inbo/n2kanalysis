#' The n2kResult class
#' @name n2kResult-class
#' @rdname n2kResult-class
#' @exportClass n2kResult
#' @aliases n2kResult-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kAnalysisMetadata_class.R
#' @include n2kAnomaly_class.R
#' @include n2kContrast_class.R
setClass(
  "n2kResult",
  contains = c("n2kAnalysisMetadata", "n2kAnomaly", "n2kContrast")
)

#' @importFrom methods setValidity
setValidity(
  "n2kResult",
  function(object) {
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
    if (!all(
      object@ContrastCoefficient$Parameter %in% object@Parameter$Fingerprint
    )) {
      stop(
"Some Parameter in 'ContrastCoefficient' slot are not present in 'Parameter'
slot"
      )
    }
    if (!all(
      object@Contrast$Analysis %in% object@AnalysisMetadata$FileFingerprint
    )) {
      stop(
"Some Analysis in 'Contrast' slot are not present in 'AnalysisMetadata' slot"
      )
    }
    return(TRUE)
  }
)
