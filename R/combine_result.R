#' @rdname combine
#' @aliases combine,n2kResult-methods
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% arrange_ distinct
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
      distinct() %>%
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
