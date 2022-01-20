#' Combine multiple n2k objects
#' @param ... n2k objects
#' @name combine
#' @rdname combine
#' @exportMethod combine
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  "combine",
  def = function(...) {
    standardGeneric("combine") # nocov
  }
)

#' @rdname combine
#' @aliases combine,n2kAnalysisVersion-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_version_class.R
setMethod(
  f = "combine",
  signature = "n2kAnalysisVersion",
  definition = function(...) {
    dots <- list(...)
    analysis_version <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisVersion
        }
      )
    ))
    analysis_version <- analysis_version[
      order(analysis_version$Fingerprint),
      ,
      drop = FALSE
    ]
    r_package <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@RPackage
        }
      )
    ))
    r_package <- r_package[order(r_package$Description, r_package$Version), ]
    analysis_version_r_package <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisVersionRPackage
        }
      )
    ))
    analysis_version_r_package <- analysis_version_r_package[
      order(
        analysis_version_r_package$AnalysisVersion,
        analysis_version_r_package$RPackage
      ),
    ]
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = analysis_version,
      RPackage = r_package,
      AnalysisVersionRPackage = analysis_version_r_package
    )
  }
)

#' @rdname combine
#' @aliases combine,n2kParameter-methods
#' @importFrom methods setMethod new
#' @include n2k_parameter_class.R
setMethod(
  f = "combine",
  signature = "n2kParameter",
  definition = function(...) {
    dots <- list(...)
    parameter <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@Parameter
        }
      )
    ))
    parameter_estimate <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@ParameterEstimate
        }
      )
    ))
    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter_estimate
    )
  }
)

#' @rdname combine
#' @aliases combine,n2kAnamoly-methods
#' @importFrom methods setMethod new
#' @include n2k_anomaly_class.R
setMethod(
  f = "combine",
  signature = "n2kAnomaly",
  definition = function(...) {
    dots <- list(...)
    anomaly_type <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnomalyType
        }
      )
    ))
    anomaly <- unique(do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@Anomaly
        }
      )
    ))
    new(
      "n2kAnomaly",
      AnomalyType = anomaly_type,
      Anomaly = anomaly
    )
  }
)
