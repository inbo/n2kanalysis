#' Get an analysis version
#' @param version the object to extract the version
#' @name get_analysis_version
#' @rdname get_analysis_version
#' @exportMethod get_analysis_version
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_analysis_version",
  def = function(version) {
    standardGeneric("get_analysis_version")
  }
)

#' @rdname get_analysis_version
#' @aliases get_analysis_version,sessionInfo-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_dataframe_variable
#' @include n2k_analysis_version_class.R
setMethod(
  f = "get_analysis_version",
  signature = signature(version = "data.frame"),
  definition = function(version) {
    check_dataframe_variable(
      df = version,
      variable = c("Description", "Version", "Origin", "Fingerprint"),
      name = "version"
    )
    stopifnot(
      "Missing analysis_version attribute" = !is.null(
        attr(version, "analysis_version")
      )
    )
    analysis_version <- data.frame(
      Fingerprint = attr(version, "analysis_version"),
      stringsAsFactors = FALSE
    )
    version <- version[order(version$Description, version$Version), ]
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = analysis_version,
      RPackage = version,
      AnalysisVersionRPackage = data.frame(
        AnalysisVersion = analysis_version$Fingerprint,
        RPackage = version$Fingerprint,
        stringsAsFactors = FALSE
      )
    )
  }
)

#' @rdname get_analysis_version
#' @aliases get_analysis_version,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "get_analysis_version",
  signature = signature(version = "n2kAnalysisMetadata"),
  definition = function(version) {
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = version@AnalysisVersion,
      Rpackage = version@Rpackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage
    )
  }
)

#' @rdname get_analysis_version
#' @aliases get_analysis_version,sessionInfo-methods
#' @importFrom methods setMethod new
#' @importFrom utils sessionInfo
#' @include import_s3_classes.R
setMethod(
  f = "get_analysis_version",
  signature = signature(version = "sessionInfo"),
  definition = function(version) {
    get_analysis_version(version = session_package(version))
  }
)
