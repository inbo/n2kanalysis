#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2k_composite_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kComposite"),
  definition = function(analysis, ...) {
    if (analysis@AnalysisMetadata$status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      description = "Composite index",
      parent = NA_character_,
      stringsAsFactors = FALSE
    )
    parameter$fingerprint <- apply(parameter, 1, sha1)

    parameter_estimate <- cbind(
      Analysis = analysis@AnalysisMetadata$file_fingerprint,
      analysis@Index,
      stringsAsFactors = FALSE
    )
    colnames(parameter_estimate)[2] <- "parameter"
    row.names(parameter_estimate) <- NULL

    extra <- data.frame(
      description = parameter_estimate$parameter,
      parent = parameter$fingerprint,
      stringsAsFactors = FALSE
    )
    extra$fingerprint <- apply(extra, 1, sha1)
    parameter <- rbind(parameter, extra)

    parameter_estimate$parameter <- extra$fingerprint

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter_estimate
    )
  }
)
