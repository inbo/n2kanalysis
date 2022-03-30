#' The `n2kResult` class
#' @name n2kResult-class
#' @rdname n2kResult-class
#' @exportClass n2kResult
#' @aliases n2kResult-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_analysis_metadata_class.R
#' @include n2k_anomaly_class.R
#' @include n2k_contrast_class.R
setClass(
  "n2kResult",
  contains = c("n2kAnalysisMetadata", "n2kAnomaly", "n2kContrast")
)

#' @importFrom methods setValidity
setValidity(
  "n2kResult",
  function(object) {
    if (!all(
      object@AnalysisMetadata$analysis_version %in%
        object@AnalysisVersion$fingerprint
    )) {
      stop(
"Some analysis_version in 'AnalysisMetadata' slot are not present in
'AnalysisVersion' slot"
      )
    }
    if (!all(
object@ParameterEstimate$analysis %in% object@AnalysisMetadata$file_fingerprint
    )) {
      stop(
"Some Analysis in 'ParameterEstimate' slot are not present in 'AnalysisMetadata'
slot"
      )
    }
    if (!all(
      object@Anomaly$analysis %in% object@AnalysisMetadata$file_fingerprint
    )) {
      stop(
"Some Analysis in 'Anomaly' slot are not present in 'AnalysisMetadata' slot"
      )
    }
    if (!all(
      object@ContrastCoefficient$parameter %in% object@Parameter$fingerprint
    )) {
      stop(
"Some Parameter in 'ContrastCoefficient' slot are not present in 'Parameter'
slot"
      )
    }
    if (!all(
      object@Contrast$analysis %in% object@AnalysisMetadata$file_fingerprint
    )) {
      stop(
"Some Analysis in 'Contrast' slot are not present in 'AnalysisMetadata' slot"
      )
    }
    return(TRUE)
  }
)
