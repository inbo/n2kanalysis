#' @rdname combine
#' @aliases combine,n2kResult-methods
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% arrange distinct
#' @importFrom stats as.formula
#' @include n2k_result_class.R
setMethod(
  f = "combine",
  signature = "n2kResult",
  definition = function(...) {
    dots <- list(...)

    analysis_metadata <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisMetadata
        }
      )
    ) %>%
      arrange(.data$FileFingerprint)

    analysis_formula <- lapply(analysis_metadata$Formula, as.formula)

    analysis_relation <- do.call(
      rbind,
      lapply(
        dots,
        function(x) {
          x@AnalysisRelation
        }
    )) %>%
      arrange(.data$Analysis, .data$ParentAnalysis)

    analysis_version <- do.call(
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

    r_package <- do.call(
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

    analysis_version_r_package <- do.call(
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

    parameter_estimate <- do.call(
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

    anomaly_type <- do.call(
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
    contrast_coefficient <- do.call(
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
    contrast_estimate <- do.call(
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
      AnalysisMetadata = analysis_metadata,
      AnalysisFormula = analysis_formula,
      AnalysisRelation = analysis_relation,
      AnalysisVersion = analysis_version,
      RPackage = r_package,
      AnalysisVersionRPackage = analysis_version_r_package,
      Parameter = parameter,
      ParameterEstimate = parameter_estimate,
      AnomalyType = anomaly_type,
      Anomaly = anomaly,
      Contrast = contrast,
      ContrastCoefficient = contrast_coefficient,
      ContrastEstimate = contrast_estimate
    )
  }
)
