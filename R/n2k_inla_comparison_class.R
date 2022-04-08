#' @importFrom methods setClassUnion
setClassUnion("maybeDataFrame", c("data.frame", "NULL"))

#' The `n2kInlaComparison` class
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
#' @include n2k_model_class.R
setClass(
  "n2kInlaComparison",
  representation = representation(WAIC = "maybeDataFrame"),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that noNA
#' @importFrom digest sha1
setValidity(
  "n2kInlaComparison",
  function(object) {
    assert_that(nrow(object@AnalysisRelation) > 1)
    assert_that(noNA(object@AnalysisRelation$parent_analysis))

    if (!grepl("^inla comparison: ", object@AnalysisMetadata$model_type)) {
      stop("model_type should be 'inla comparison:'")
    }

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
      )
    )
    if (object@AnalysisMetadata$file_fingerprint != file_fingerprint) {
      stop("Corrupt file_fingerprint")
    }

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status, object@WAIC,
        object@AnalysisMetadata$analysis_version, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage, object@AnalysisRelation
      ),
      digits = 6L
    )
    if (object@AnalysisMetadata$status_fingerprint != status_fingerprint) {
      stop("Corrupt status_fingerprint")
    }

    return(TRUE)
  }
)
