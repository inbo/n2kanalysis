#' @rdname get_anomaly
#' @importFrom methods setMethod new
#' @include n2k_model_class.R
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kModel"),
  definition = function(analysis, verbose = TRUE, ...) {
    parameter <- get_model_parameter(analysis = analysis, verbose = verbose)
    return(
      new(
        "n2kAnomaly",
        Parameter = parameter@Parameter,
        ParameterEstimate = parameter@ParameterEstimate
      )
    )
  }
)
