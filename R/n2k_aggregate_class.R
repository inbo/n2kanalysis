#' @importFrom methods setClassUnion
#' @importClassesFrom multimput aggregatedImputed
#' @include n2k_inla_class.R
setClassUnion("maybeAggregatedImputed", c("aggregatedImputed", "NULL"))

#' The n2kAggregate class
#'
#' It holds analysis data based on an aggregated imputation
#' @section Slots:
#'   \describe{
#'    \item{\code{RawImputed}}{A \code{rawImputed} object with multiple
#'    imputations.}
#'    \item{\code{Function}}{The function to apply on each group.}
#'    \item{\code{Filter}}{The object to pass to the \code{filter} argument of
#'    \code{\link[multimput]{aggregate_impute}}.}
#'    \item{\code{Join}}{The object to pass to the \code{join} argument of
#'    \code{\link[multimput]{aggregate_impute}}.}
#'    \item{\code{AggregatedImputed}}{An \code{aggregatedImputed} object with
#'    multiple imputations.}
#'   }
#' @name n2kAggregate-class
#' @rdname n2kAggregate-class
#' @exportClass n2kAggregate
#' @aliases n2kAggregate-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_model_class.R
setClass(
  "n2kAggregate",
  representation = representation(
    RawImputed = "maybeRawImputed",
    Function = "function",
    Filter = "list",
    Join = "list",
    AggregatedImputed = "maybeAggregatedImputed"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kAggregate",
  function(object) {
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
        object@AnalysisRelation$parent_analysis,
        object@Function, object@Filter, object@Join
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
        object@AnalysisRelation, object@RawImputed, object@AggregatedImputed
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
