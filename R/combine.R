#' Combine multiple n2k objects
#' @param ... n2k objects
#' @name combine
#' @rdname combine
#' @exportMethod combine
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  "combine",
  def = function(...){
    standardGeneric("combine")
  }
)

#' @rdname combine
#' @aliases combine,n2kAnalysisVersion-methods
#' @importFrom methods setMethod
#' @include n2kAnalysisVersion_class.R
setMethod(
  f = "combine",
  signature = "n2kAnalysisVersion",
  definition = function(...){
    dots <- list(...)
    analysis.version <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnalysisVersion})
    ))
    analysis.version <- analysis.version[order(analysis.version$Fingerprint), , drop = FALSE]
    r.package <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@RPackage})
    ))
    r.package <- r.package[order(r.package$Description, r.package$Version), ]
    analysis.version.r.package <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnalysisVersionRPackage})
    ))
    analysis.version.r.package <- analysis.version.r.package[
      order(analysis.version.r.package$AnalysisVersion, analysis.version.r.package$RPackage), 
    ]
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = analysis.version,
      RPackage = r.package,
      AnalysisVersionRPackage = analysis.version.r.package
    )
  }
)

#' @rdname combine
#' @aliases combine,n2kParameter-methods
#' @importFrom methods setMethod
#' @include n2kParameter_class.R
setMethod(
  f = "combine",
  signature = "n2kParameter",
  definition = function(...){
    dots <- list(...)
    parameter <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@Parameter})
    ))
    parameter.estimate <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@ParameterEstimate})
    ))
    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)

#' @rdname combine
#' @aliases combine,n2kAnamoly-methods
#' @importFrom methods setMethod
#' @include n2kAnomaly_class.R
setMethod(
  f = "combine",
  signature = "n2kAnomaly",
  definition = function(...){
    dots <- list(...)
    anomaly.type <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnomalyType})
    ))
    anomaly <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@Anomaly})
    ))
    new(
      "n2kAnomaly",
      AnomalyType = anomaly.type,
      Anomaly = anomaly
    )
  }
)
