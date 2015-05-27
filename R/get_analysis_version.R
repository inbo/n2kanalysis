#' Get an analysis version
#' @param version the object to extract the version
#' @name get_analysis_version
#' @rdname get_analysis_version
#' @exportMethod get_analysis_version
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_analysis_version", 
  def = function(version){
    standard.generic("get_analysis_version")
  }
)

#' @rdname get_analysis_version
#' @aliases get_analysis_version,sessionInfo-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_dataframe_variable
#' @include n2kAnalysisVersion_class.R
setMethod(
  f = "get_analysis_version",
  signature = signature(version = "data.frame"),
  definition = function(version){
    check_dataframe_variable(
      df = version, 
      variable = c("Description", "Version", "Fingerprint"), 
      name = "version"
    )
    if(is.null(attr(version, "AnalysisVersion"))){
      stop("Missing AnalysisVersion attribute")
    }
    analysis.version <- data.frame(
      Fingerprint = attr(version, "AnalysisVersion"),
      stringsAsFactors = FALSE
    )
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = analysis.version,
      RPackage = version,
      AnalysisVersionRPackage = data.frame(
        AnalysisVersion = analysis.version$Fingerprint,
        RPackage = version$Fingerprint,
        stringsAsFactors = FALSE
      )
    )
  }
)

#' @rdname get_analysis_version
#' @aliases get_analysis_version,n2kModel-methods
#' @importFrom methods setMethod
#' @include n2kModel_class.R
setMethod(
  f = "get_analysis_version",
  signature = signature(version = "n2kModel"),
  definition = function(version){
    get_analysis_version(version = session_package(version))
  }
)

#' @rdname get_analysis_version
#' @aliases get_analysis_version,sessionInfo-methods
#' @importFrom methods setMethod
#' @include n2kModel_class.R
setMethod(
  f = "get_analysis_version",
  signature = signature(version = "sessionInfo"),
  definition = function(version){
    get_analysis_version(version = session_package(version))
  }
)
