#' simplify an n2kResult object
#'
#' \itemize{
#'  \item Convert FileFingerprint to factor
#'  \item Convert all other fingerprints to integer
#'  \item Convert most of the characters to factors
#' }
#' @export
#' @include n2kResult_class.R
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% select_ distinct arrange_ mutate_ anti_join inner_join
#' @param result a n2kResult object
simplify_result <- function(result){
  assert_that(inherits(result, "n2kResult"))
  validObject(result)

  # convert analysis fingerprint from sha1 to factor
  analysis.level <- c(
    result@AnalysisMetadata$FileFingerprint,
    result@AnalysisRelation$ParentAnalysis
  )
  analysis.level <- sort(unique(analysis.level))

  result@AnalysisRelation$Analysis <- factor(
    result@AnalysisRelation$Analysis,
    levels = analysis.level
  )
  result@AnalysisRelation$ParentAnalysis <- factor(
    result@AnalysisRelation$ParentAnalysis,
    levels = analysis.level
  )
  result@ParameterEstimate$Analysis <- factor(
    result@ParameterEstimate$Analysis,
    levels = analysis.level
  )
  result@Anomaly$Analysis <- factor(
    result@Anomaly$Analysis,
    levels = analysis.level
  )
  result@Contrast$Analysis <- factor(
    result@Contrast$Analysis,
    levels = analysis.level
  )
  result@AnalysisMetadata$FileFingerprint <- factor(
    result@AnalysisMetadata$FileFingerprint,
    levels = analysis.level
  )

  # convert parameter fingerprint from sha1 to integer
  parameter <- result@Parameter %>%
    select_(~Fingerprint) %>%
    distinct() %>%
    arrange_(~Fingerprint) %>%
    mutate_(ParameterID = ~seq_along(Fingerprint))

  antijoin.ap <- result@Anomaly %>%
    anti_join(parameter, by = c("Parameter" = "Fingerprint"))
  assert_that(nrow(antijoin.ap) == 0)
  result@Anomaly <- result@Anomaly %>%
    inner_join(parameter, by = c("Parameter" = "Fingerprint")) %>%
    select_(~-Parameter, Parameter = ~ParameterID)

  antijoin.pep <- result@ParameterEstimate %>%
    anti_join(parameter, by = c("Parameter" = "Fingerprint"))
  assert_that(nrow(antijoin.pep) == 0)
  result@ParameterEstimate <- result@ParameterEstimate %>%
    inner_join(parameter, by = c("Parameter" = "Fingerprint")) %>%
    select_(~-Parameter, Parameter = ~ParameterID)

  antijoin.ccp <- result@ContrastCoefficient %>%
    anti_join(parameter, by = c("Parameter" = "Fingerprint"))
  assert_that(nrow(antijoin.ccp) == 0)
  result@ContrastCoefficient <- result@ContrastCoefficient %>%
    inner_join(parameter, by = c("Parameter" = "Fingerprint")) %>%
    select_(~-Parameter, Parameter = ~ParameterID)

  antijoin.pp <- result@Parameter %>%
    anti_join(parameter, by = "Fingerprint")
  assert_that(nrow(antijoin.pp) == 0)
  antijoin.pp2 <- result@Parameter %>%
    filter_(~!is.na(Parent)) %>%
    anti_join(parameter, by = c("Parent" = "Fingerprint"))
  assert_that(nrow(antijoin.pp2) == 0)
  result@Parameter <- result@Parameter %>%
    inner_join(parameter, by = "Fingerprint") %>%
    select_(~-Fingerprint, Fingerprint = ~ParameterID) %>%
    left_join(parameter, by = c("Parent" = "Fingerprint")) %>%
    select_(~-Parent, Parent = ~ParameterID)

  # convert contrast fingerprint from sha1 to integer
  contrast.level <- sort(unique(result@Contrast$Fingerprint))
  result@ContrastCoefficient$Contrast <- as.integer(factor(
    result@ContrastCoefficient$Contrast,
    levels = contrast.level
  ))
  result@ContrastEstimate$Contrast <- as.integer(factor(
    result@ContrastEstimate$Contrast,
    levels = contrast.level
  ))
  result@Contrast$Fingerprint <- as.integer(factor(
    result@Contrast$Fingerprint,
    levels = contrast.level
  ))

  # convert anomaly type fingerprint from sha1 to integer
  anoamytype.level <- sort(unique(result@AnomalyType$Fingerprint))
  result@Anomaly$AnomalyType <- as.integer(factor(
    result@Anomaly$AnomalyType,
    levels = anoamytype.level
  ))
  result@AnomalyType$Fingerprint <- as.integer(factor(
    result@AnomalyType$Fingerprint,
    levels = anoamytype.level
  ))

  # convert R package fingerprint from sha1 to integer
  rpackage.level <- sort(unique(result@RPackage$Fingerprint))
  result@AnalysisVersionRPackage$RPackage <- as.integer(factor(
    result@AnalysisVersionRPackage$RPackage,
    levels = rpackage.level
  ))
  result@RPackage$Fingerprint <- as.integer(factor(
    result@RPackage$Fingerprint,
    levels = rpackage.level
  ))

  # convert analysis version fingerprint to factor
  version.level <- sort(unique(result@AnalysisVersion$Fingerprint))
  result@AnalysisMetadata$AnalysisVersion <- factor(
    result@AnalysisMetadata$AnalysisVersion,
    levels = version.level
  )
  result@AnalysisVersionRPackage$AnalysisVersion <- factor(
    result@AnalysisVersionRPackage$AnalysisVersion,
    levels = version.level
  )
  result@AnalysisVersion$Fingerprint <- factor(
    result@AnalysisVersion$Fingerprint,
    levels = version.level
  )

  # convert observationID to factor
  result@AnalysisRelation$ParentStatusFingerprint <- factor(
    result@AnalysisRelation$ParentStatusFingerprint
  )
  result@AnalysisRelation$ParentStatus <- factor(
    result@AnalysisRelation$ParentStatus
  )
  result@AnalysisMetadata$Status <- factor(
    result@AnalysisMetadata$Status
  )
  result@AnalysisMetadata$Formula <- factor(
    result@AnalysisMetadata$Formula
  )
  result@AnalysisMetadata$ModelType <- factor(
    result@AnalysisMetadata$ModelType
  )
  result@Anomaly$Datafield <- factor(result@Anomaly$Datafield)

  validObject(result)
  return(result)
}
