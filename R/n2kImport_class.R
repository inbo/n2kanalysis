#' The n2kImport class
#'
#' It hold the imported datasets
#' @section Slots:
#'   \describe{
#'    \item{\code{Dataset}}{A \code{data.frame} with the datasets.}
#'   }
#' @name n2kImport-class
#' @rdname n2kImport-class
#' @exportClass n2kImport
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kImport",
  representation = representation(
    Dataset = "data.frame"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kImport",
  function(object) {
    assert_that(has_name(object@Dataset, "fingerprint"))
    assert_that(has_name(object@Dataset, "filename"))
    assert_that(has_name(object@Dataset, "import_date"))

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
        format(object@AnalysisMetadata$AnalysisDate, tz = "UTC"),
        object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis
      ),
      environment = FALSE
    )

    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@AnalysisMetadata$AnalysisVersion, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@Dataset
      ),
      digits = 6L
    )

    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
