#' @importFrom methods setClassUnion
setClassUnion("maybeDataFrame", c("data.frame", "NULL"))

#' The n2kInlaComparison class
#'
#' Compare multiple models using the WAIC criterion
#' @section Slots:
#'   \describe{
#'    \item{\code{WAIC}}{A \code{data.frame} with WAIC values per model.}
#'   }
#' @name n2kInlaComparison-class
#' @rdname n2kInlaComparison-class
#' @exportClass n2kInlaComparison
#' @aliases n2kInlaComparison-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kInlaComparison",
  representation = representation(
    WAIC = "maybeDataFrame"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that noNA
#' @importFrom digest sha1
setValidity(
  "n2kInlaComparison",
  function(object) {
    assert_that(nrow(object@AnalysisRelation) > 1)
    assert_that(noNA(object@AnalysisRelation$ParentAnalysis))

    if (!grepl("^inla comparison: ", object@AnalysisMetadata$ModelType)) {
      stop("ModelType should be 'inla comparison:'")
    }

    file_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$ResultDatasourceID,
        object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Formula,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        format(object@AnalysisMetadata$AnalysisDate, tz = "UTC"),
        object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file_fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@WAIC, object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      ),
      digits = 6L
    )
    if (object@AnalysisMetadata$StatusFingerprint != status_fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
