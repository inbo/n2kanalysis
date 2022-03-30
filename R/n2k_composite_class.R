#' The `n2kComposite` class
#'
#' Calculate composite indices from multiple analysis
#' @section Slots:
#'   \describe{
#'    \item{\code{Extractor}}{A function to extract the relevant parameters from
#'    the model.}
#'    \item{\code{Parameter}}{A data.frame with the relevant parameter estimates
#'    of each parent analysis.}
#'    \item{\code{Index}}{The composite index based on the parameters}
#'   }
#' @name n2kComposite-class
#' @rdname n2kComposite-class
#' @exportClass n2kComposite
#' @aliases n2kComposite-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_model_class.R
setClass(
  "n2kComposite",
  representation = representation(
    Extractor = "function",
    Parameter = "data.frame",
    Index = "data.frame"
  ),
  contains = "n2kModel"
)


#' @importFrom methods setValidity
#' @importFrom digest sha1
setValidity(
  "n2kComposite",
  function(object) {
    assert_that(
      nrow(object@AnalysisRelation) > 0, msg = "'AnalysisRelation' not defined"
    )
    assert_that(
      noNA(object@AnalysisRelation$parent_analysis),
      msg = "'parent_analysis' in 'AnalysisRelation' slot cannot be missing"
    )

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
        object@AnalysisMetadata$seed, object@AnalysisRelation$parent_analysis,
        formals(object@Extractor), as.character(body(object@Extractor))
      )
    )
    assert_that(
      object@AnalysisMetadata$file_fingerprint == file_fingerprint,
      msg = "Corrupt file_fingerprint"
    )
    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status, object@Parameter, object@Index,
        object@AnalysisMetadata$analysis_version, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage, object@AnalysisRelation
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
