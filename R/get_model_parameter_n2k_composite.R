#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2k_composite_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kComposite"),
  definition = function(analysis, ...) {
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "Composite index",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, sha1)

    parameter_estimate <- cbind(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      analysis@Index,
      stringsAsFactors = FALSE
    )
    colnames(parameter_estimate)[2] <- "Parameter"
    row.names(parameter_estimate) <- NULL

    extra <- data.frame(
      Description = parameter_estimate$Parameter,
      Parent = parameter$Fingerprint,
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, sha1)
    parameter <- rbind(parameter, extra)

    parameter_estimate$Parameter <- extra$Fingerprint

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter_estimate
    )
  }
)
