#' @rdname combine
#' @aliases combine,n2kResult-methods
#' @importFrom methods setMethod
#' @include n2kResult_class.R
setMethod(
  f = "combine",
  signature = "n2kResult",
  definition = function(...){
    dots <- list(...)
    
    analysis.metadata <- do.call(
      rbind, 
      lapply(dots, function(x){x@AnalysisMetadata}
    ))
    analysis.version <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnalysisVersion})
    ))
    r.package <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@RPackage})
    ))
    analysis.version.r.package <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnalysisVersionRPackage})
    ))
    parameter <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@Parameter})
    ))
    parameter.estimate <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@ParameterEstimate})
    ))
    anomaly.type <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnomalyType})
    ))
    anomaly <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@Anomaly})
    ))
    
    new(
      "n2kResult",
      AnalysisVersion = analysis.version,
      RPackage = r.package,
      AnalysisVersionRPackage = analysis.version.r.package,
      AnalysisMetadata = new("n2kAnalysisMetadata", analysis.metadata),
      Parameter = parameter,
      ParameterEstimate = parameter.estimate,
      AnomalyType = anomaly.type,
      Anomaly = anomaly
    )
  }
)
