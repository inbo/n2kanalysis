#' @rdname get_anomaly
#' @importFrom methods setMethod new
#' @include n2kModel_class.R
#' @importFrom assertthat assert_that is.flag noNA
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kModel"),
  definition = function(analysis, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

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
