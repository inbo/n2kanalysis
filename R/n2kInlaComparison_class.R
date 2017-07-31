#' @importFrom methods setClassUnion
setClassUnion("maybeDataFrame", c("data.frame", "NULL"))

#' The n2kInlaComparison class
#'
#' Compare multiple models using the WAIC criterion
#' @section Slots:
#'   \describe{
#'    \item{\code{Models}}{a list of INLA models}
#'    \item{\code{WAIC}}{a data.frame with WAIC values per model}
#'   }
#' @name n2kInlaComparison-class
#' @rdname n2kInlaComparison-class
#' @exportClass n2kInlaComparison
#' @aliases n2kInlaComparison-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R n2kGlmerPoisson_class.R
setClass(
  "n2kInlaComparison",
  representation = representation(
    Models = "list",
    WAIC = "maybeDataFrame"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that noNA
#' @importFrom digest sha1
setValidity(
  "n2kInlaComparison",
  function(object){
    assert_that(nrow(object@AnalysisRelation) > 1)
    assert_that(length(object@Models) <= nrow(object@AnalysisRelation))
    assert_that(noNA(object@AnalysisRelation$ParentAnalysis))

    if (!grepl("^inla comparison: ", object@AnalysisMetadata$ModelType)) {
      stop("ModelType should be 'inla comparison:'")
    }

    file.fingerprint <- sha1(
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
        object@AnalysisMetadata$AnalysisDate, object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@Models, object@WAIC,
        object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      ),
      digits = 6L
    )
    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
