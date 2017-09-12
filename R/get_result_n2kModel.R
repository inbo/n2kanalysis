#' @rdname get_result
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag noNA
#' @include n2kModel_class.R
#' @include n2kResult_class.R
#' @param verbose Print extra information on the screen
setMethod(
  f = "get_result",
  signature = signature(x = "n2kModel"),
  definition = function(x, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    validObject(x)
    anomaly <- get_anomaly(analysis = x, verbose = verbose, ...)
    return(
      new(
        "n2kResult",
        AnalysisMetadata = x@AnalysisMetadata,
        AnalysisFormula = lapply(x@AnalysisMetadata$Formula, as.formula),
        AnalysisRelation = x@AnalysisRelation,
        AnalysisVersion = x@AnalysisVersion,
        RPackage = x@RPackage,
        AnalysisVersionRPackage = x@AnalysisVersionRPackage,
        Parameter = anomaly@Parameter,
        ParameterEstimate = anomaly@ParameterEstimate,
        AnomalyType = anomaly@AnomalyType,
        Anomaly = anomaly@Anomaly
      )
    )
  }
)
