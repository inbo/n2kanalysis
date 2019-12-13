#' @rdname combine
#' @aliases combine,n2kResult-methods
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% arrange distinct
#' @importFrom stats as.formula
#' @include n2kResult_class.R
setMethod(
  f = "combine",
  signature = "n2kResult",
  definition = function(...) {
    dots <- list(...)

    analysis.metadata <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisMetadata
        }
      )
    ) %>%
      arrange(.data$FileFingerprint)

    analysis.formula <- lapply(analysis.metadata$Formula, as.formula)

    analysis.relation <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisRelation
        }
    )) %>%
      arrange(.data$Analysis, .data$ParentAnalysis)

    analysis.version <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisVersion
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Fingerprint)

    r.package <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@RPackage
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Fingerprint)

    analysis.version.r.package <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisVersionRPackage
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$AnalysisVersion, .data$RPackage)

    parameter <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@Parameter
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Fingerprint)

    parameter.estimate <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@ParameterEstimate
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Analysis, .data$Parameter)

    anomaly.type <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnomalyType
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Fingerprint)

    anomaly <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@Anomaly
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Analysis, .data$AnomalyType, .data$Parameter)

    contrast <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@Contrast
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Fingerprint)
    contrast.coefficient <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@ContrastCoefficient
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Contrast, .data$Parameter)
    contrast.estimate <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@ContrastEstimate
        }
      )
    ) %>%
      distinct() %>%
      arrange(.data$Contrast)

    new(
      "n2kResult",
      AnalysisMetadata = analysis.metadata,
      AnalysisFormula = analysis.formula,
      AnalysisRelation = analysis.relation,
      AnalysisVersion = analysis.version,
      RPackage = r.package,
      AnalysisVersionRPackage = analysis.version.r.package,
      Parameter = parameter,
      ParameterEstimate = parameter.estimate,
      AnomalyType = anomaly.type,
      Anomaly = anomaly,
      Contrast = contrast,
      ContrastCoefficient = contrast.coefficient,
      ContrastEstimate = contrast.estimate
    )
  }
)
