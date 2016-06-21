#' @rdname combine
#' @aliases combine,n2kResult-methods
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% arrange_ distinct_
#' @importFrom stats as.formula
#' @include n2kResult_class.R
setMethod(
  f = "combine",
  signature = "n2kResult",
  definition = function(...){
    dots <- list(...)

    analysis.metadata <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@AnalysisMetadata
        }
      )
    ) %>%
      arrange_(~FileFingerprint)

    analysis.formula <- lapply(analysis.metadata$Formula, as.formula)

    analysis.relation <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@AnalysisRelation
        }
    )) %>%
      arrange_(~Analysis, ~ParentAnalysis)

    analysis.version <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@AnalysisVersion
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Fingerprint)

    r.package <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@RPackage
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Fingerprint)

    analysis.version.r.package <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@AnalysisVersionRPackage
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~AnalysisVersion, ~RPackage)

    parameter <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@Parameter
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Fingerprint)

    parameter.estimate <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@ParameterEstimate
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Analysis, ~Parameter)

    anomaly.type <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@AnomalyType
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Fingerprint)

    anomaly <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@Anomaly
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Analysis, ~AnomalyType, ~Parameter)

    contrast <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@Contrast
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Fingerprint)
    contrast.coefficient <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@ContrastCoefficient
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Contrast, ~Parameter)
    contrast.estimate <- do.call(
      rbind,
      lapply(
        dots,
        function(x){
          x@ContrastEstimate
        }
      )
    ) %>%
      distinct_() %>%
      arrange_(~Contrast)

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
