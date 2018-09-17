#' The n2kImport class
#'
#' It hold the imported datasets
#' @section Slots:
#'   \describe{
#'    \item{\code{Dataset}}{a data.frame with the datasets}
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
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kImport",
  function(object){
    check_dataframe_variable(
      df = object@Dataset[1, ],
      variable = c("fingerprint", "filename", "import_date"),
      error = TRUE
    )
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
      )
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
