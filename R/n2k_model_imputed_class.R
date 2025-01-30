#' @importFrom methods setClassUnion
setClassUnion("maybeFunction", c("function", "character"))

#' The `n2kModelImputed` class
#'
#' It holds the model of aggregated imputed data
#' @slot Function The object to pass to the `model.fun` argument of
#' [multimput::model_impute()].
#' @slot Package A vector of package names which must be loaded to run the
#' function.
#' @slot ModelArgs The object to pass to the `model.args` argument of
#' [multimput::model_impute()].
#' @slot PrepareModelArgs An optional list containing a single function that
#' will be applied to the object.
#' The result of the function will be appended to the `ModelsArgs`.
#' @slot Extractor The object to pass to the `extractor` argument of
#' [multimput::model_impute()].
#' @slot ExtractorArgs The object to pass to the `extractor.args` argument of
#' [multimput::model_impute()].
#' @slot Filter The object to pass to the `filter` argument of
#' [multimput::model_impute()].
#' @slot Mutate The object to pass to the `mutate` argument of
#' [multimput::model_impute()].
#' @slot AggregatedImputed An `aggregatedImputed` object with multiple
#' imputations.
#' @slot Results The `data.frame` with the results of
#' [multimput::model_impute()].
#' @name n2kModelImputed-class
#' @rdname n2kModelImputed-class
#' @exportClass n2kModelImputed
#' @aliases n2kModelImputed-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_aggregate_class.R
#' @include n2k_inla_comparison_class.R
setClass(
  "n2kModelImputed",
  representation = representation(
    Function = "maybeFunction",
    Package = "character",
    ModelArgs = "list",
    PrepareModelArgs = "list",
    Extractor = "function",
    ExtractorArgs = "list",
    Filter = "list",
    Mutate = "list",
    AggregatedImputed = "maybeAggregatedImputed",
    Results = "maybeDataFrame"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name is.string noNA
setValidity(
  "n2kModelImputed",
  function(object) {
    stopifnot(
      "Function must be either a function or a string" =
        inherits(object@Function, "function") ||
        (is.string(object@Function) && noNA(object@Function))
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
        object@AnalysisMetadata$seed,
        object@AnalysisRelation$parent_analysis,
        object@Function, object@Filter, object@Mutate, object@ModelArgs,
        object@PrepareModelArgs, object@Extractor, object@ExtractorArgs,
        object@Package
      ),
      environment = FALSE
    )

    stopifnot(
      "Corrupt file_fingerprint" =
        object@AnalysisMetadata$file_fingerprint == file_fingerprint
    )

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status,
        object@AnalysisMetadata$analysis_version, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@AggregatedImputed, object@Results
      ),
      digits = 6L
    )

    stopifnot(
      "Corrupt status_fingerprint" =
        object@AnalysisMetadata$status_fingerprint == status_fingerprint
    )

    return(TRUE)
  }
)
