#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kLrtGlmer"),
  definition = function(analysis, ...) {
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "LRT test",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, sha1)

    parameter.estimate <- data.frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parameter = parameter$Fingerprint,
      Estimate = analysis@Anova[2, "Pr(>Chisq)"],
      LowerConfidenceLimit = NA_real_,
      UpperConfidenceLimit = NA_real_,
      stringsAsFactors = FALSE
    )
    row.names(parameter.estimate) <- NULL

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)
