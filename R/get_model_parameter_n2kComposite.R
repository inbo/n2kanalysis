#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kComposite_class.R
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

    parameter.estimate <- cbind(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      analysis@Index,
      stringsAsFactors = FALSE
    )
    colnames(parameter.estimate)[2] <- "Parameter"
    row.names(parameter.estimate) <- NULL

    extra <- data.frame(
      Description = parameter.estimate$Parameter,
      Parent = parameter$Fingerprint,
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, sha1)
    parameter <- rbind(parameter, extra)

    parameter.estimate$Parameter <- extra$Fingerprint

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)
