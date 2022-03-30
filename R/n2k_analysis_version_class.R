#' The `n2kAnalysisVersion` class
#' @name n2kAnalysisVersion-class
#' @rdname n2kAnalysisVersion-class
#' @exportClass n2kAnalysisVersion
#' @aliases n2kAnalysisVersion-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kAnalysisVersion",
  representation = representation(
    AnalysisVersion = "data.frame", RPackage = "data.frame",
    AnalysisVersionRPackage = "data.frame"
  ),
  prototype = prototype(
    AnalysisVersion = data.frame(
      fingerprint = character(0), stringsAsFactors = FALSE
    ),
    RPackage = data.frame(
      fingerprint = character(0), description = character(0),
      version = character(0), origin = character(0), stringsAsFactors = FALSE
    ),
    AnalysisVersionRPackage = data.frame(
      analysis_version = character(0), r_package = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom n2khelper check_dataframe_variable
#' @importFrom methods setValidity
setValidity(
  "n2kAnalysisVersion",
  function(object) {
    check_dataframe_variable(
      df = object@AnalysisVersion, variable = "fingerprint",
      name = "AnalysisVersion"
    )
    check_dataframe_variable(
      df = object@RPackage,
      variable = c("fingerprint", "description", "version", "origin"),
      name = "AnalysisVersionRPackage"
    )
    check_dataframe_variable(
      df = object@AnalysisVersionRPackage,
      variable = c("analysis_version", "r_package"),
      name = "AnalysisVersionRPackage"
    )

    if (!all(
      object@AnalysisVersionRPackage$analysis_version %in%
        object@AnalysisVersion$fingerprint
    )) {
      stop(
"Some AnalysisVersion in 'AnalysisVersionRPackage' slot are not present in
'AnalysisVersion' slot"
      )
    }
    if (!all(
      object@AnalysisVersionRPackage$r_package %in% object@RPackage$fingerprint
    )) {
      stop(
"Some r_package in 'AnalysisVersionRPackage' slot are not present in
'RPackage' slot"
      )
    }
    if (anyDuplicated(object@AnalysisVersionRPackage)) {
      stop("Duplicated rows in 'AnalysisVersionRPackage' slot")
    }
    if (anyDuplicated(object@AnalysisVersion)) {
      stop("Duplicated rows in 'AnalysisVersion' slot")
    }
    if (anyDuplicated(object@RPackage)) {
      stop("Duplicated rows in 'RPackage' slot")
    }
    return(TRUE)
  }
)
