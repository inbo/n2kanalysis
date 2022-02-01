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

    analysis_metadata <- lapply(dots, slot, "AnalysisMetadata") %>%
      do.call(what = rbind) %>%
      arrange(.data$file_fingerprint)

    analysis_formula <- lapply(analysis_metadata$formula, as.formula)

    analysis_relation <- lapply(dots, slot, "AnalysisRelation") %>%
      do.call(what = rbind) %>%
      arrange(.data$analysis, .data$parent_analysis)

    analysis_version <- lapply(dots, slot, "AnalysisVersion") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$fingerprint)

    r_package <- lapply(dots, slot, name = "RPackage") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$fingerprint)

    analysis_version_r_package <- lapply(
      dots, slot, name = "AnalysisVersionRPackage"
    ) %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$analysis_version, .data$r_package)

    parameter <- lapply(dots, slot, "Parameter") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$fingerprint)

    parameter_estimate <- lapply(dots, slot, "ParameterEstimate") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$analysis, .data$parameter)

    anomaly_type <- lapply(dots, slot, "AnomalyType") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$fingerprint)

    anomaly <- lapply(dots, slot, "Anomaly") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$analysis, .data$anomaly_type, .data$parameter)

    contrast <- lapply(dots, slot, "Contrast") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$fingerprint)
    contrast_coefficient <- lapply(dots, slot, "ContrastCoefficient") %>%
      do.call(what = rbind) %>%
      distinct() %>%
      arrange(.data$Contrast, .data$parameter)
    contrast_estimate <- lapply(dots, slot, "ContrastEstimate") %>%
      do.call(what = rbind) %>%
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
