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
#' @include n2k_model_class.R
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

    file_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$result_datasource_id,
        object@AnalysisMetadata$scheme_id,
        object@AnalysisMetadata$species_group_id,
        object@AnalysisMetadata$location_group_id,
        object@AnalysisMetadata$model_type, object@AnalysisMetadata$formula,
        object@AnalysisMetadata$first_imported_year,
        object@AnalysisMetadata$last_imported_year,
        object@AnalysisMetadata$duration,
        object@AnalysisMetadata$last_analysed_year,
        format(object@AnalysisMetadata$analysis_date, tz = "UTC"),
        object@AnalysisMetadata$seed,
        object@AnalysisRelation$parent_analysis
      ),
      environment = FALSE
    )

    assert_that(
      object@AnalysisMetadata$file_fingerprint == file_fingerprint,
      msg = "Corrupt file_fingerprint"
    )

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status,
        object@AnalysisMetadata$analysis_version, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@Dataset
      ),
      digits = 6L
    )

    assert_that(
      object@AnalysisMetadata$status_fingerprint == status_fingerprint,
      msg = "Corrupt status_fingerprint"
    )

    return(TRUE)
  }
)
