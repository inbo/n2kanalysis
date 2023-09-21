#' Create an `n2kModelImputed` object
#' @inheritParams n2k_inla
#' @template analysis_metadata
#' @details
#' - `model_fun`: The `model_fun` argument of [multimput::model_impute()].
#' - `package`: A character vector of package names which must be loaded for
#' `model_fun`.
#' - `model_args`: An optional list for the `model_args` argument of
#'  [multimput::model_impute()].
#' - `extractor`: An optional list for the `extractor` argument of
#'  [multimput::model_impute()].
#' - `extractor_args`: An optional list for the `extractor_args` argument of
#'  [multimput::model_impute()].
#' - `filter`: An optional list for the `filter` argument of
#'  [multimput::model_impute()].
#' - `mutate`: An optional list for the `mutate`` argument of
#'  [multimput::model_impute()].
#' @name n2k_model_imputed
#' @rdname n2k_model_imputed
#' @exportMethod n2k_model_imputed
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_model_imputed",
  def = function(
    ...
  ) {
    standardGeneric("n2k_model_imputed") # nocov
  }
)

#' @description A new `n2kModelImputed` model.
#' @rdname n2k_model_imputed
#' @aliases n2k_model_imputed,n2kModelImputed-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_model_imputed_class.R
setMethod(
  f = "n2k_model_imputed",
  signature = signature("ANY"),
  definition = function(
    ...
  ) {
    dots <- list(...)
    #set the defaults for missing arguments in dots
    if (is.null(dots$status)) {
      dots$status <- "waiting"
    }
    if (is.null(dots$seed)) {
      dots$seed <- sample(.Machine$integer.max, 1)
    }
    assert_that(
      is.count(dots$seed), is.string(dots$result_datasource_id),
      is.string(dots$scheme_id), is.string(dots$species_group_id),
      is.string(dots$location_group_id), is.string(dots$model_type),
      is.string(dots$formula), is.count(dots$first_imported_year),
      is.count(dots$last_imported_year), is.time(dots$analysis_date)
    )
    dots$seed <- as.integer(dots$seed)
    dots$first_imported_year <- as.integer(dots$first_imported_year)
    dots$last_imported_year <- as.integer(dots$last_imported_year)
    dots$duration <- coalesce(
      dots$duration, dots$last_imported_year - dots$first_imported_year + 1L
    )
    dots$last_analysed_year <- coalesce(
      dots$last_analysed_year, dots$last_imported_year
    )
    dots$filter <- coalesce(dots$filter, list())
    dots$mutate <- coalesce(dots$mutate, list())
    dots$model_args <- coalesce(dots$model_args, list())
    dots$prepare_model_args <- coalesce(dots$prepare_model_args, list())
    dots$extractor_args <- coalesce(dots$extractor_args, list())
    dots$package <- c(dots$package, character(0))
    assert_that(
      is.count(dots$duration), is.count(dots$last_analysed_year),
      is.list(dots$filter), is.list(dots$mutate), is.list(dots$model_args),
      is.function(dots$model_fun) || is.string(dots$model_fun),
      is.function(dots$extractor), is.list(dots$prepare_model_args),
      length(dots$prepare_model_args) <= 1, is.list(dots$extractor_args),
      is.character(dots$package), is.string(dots$parent)
    )
    dots$duration <- as.integer(dots$duration)
    dots$last_analysed_year <- as.integer(dots$last_analysed_year)
    if (length(dots$prepare_model_args)) {
      assert_that(is.function(dots$prepare_model_args[[1]]))
    }

    file_fingerprint <- sha1(
      list(
        dots$result_datasource_id,
        dots$scheme_id, dots$species_group_id, dots$location_group_id,
        dots$model_type, dots$formula, dots$first_imported_year,
        dots$last_imported_year, dots$duration, dots$last_analysed_year,
        format(dots$analysis_date, tz = "UTC"),
        dots$seed, dots$parent, dots$model_fun, dots$filter,
        dots$mutate, dots$model_args, dots$prepare_model_args, dots$extractor,
        dots$extractor_args, dots$package
      ),
      environment = FALSE
    )

    if (is.null(dots$parent_statusfingerprint)) {
      dots$parent_status <- coalesce(dots$parent_status, "waiting")
      dots$parent_statusfingerprint <- sha1(dots$parent_status)
    }
    stopifnot(
"'parent_status' is required when 'parent_statusfingerprint' is provided" =
!is.null(dots[["parent_status"]])
    )
    analysis_relation <- data.frame(
      analysis = file_fingerprint, parent_analysis = dots$parent,
      parentstatus_fingerprint = dots$parent_statusfingerprint,
      parent_status = dots$parent_status, stringsAsFactors = FALSE
    )
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, dots$status, version@AnalysisVersion$fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, analysis_relation, NULL, NULL
      ),
      digits = 6L
    )

    new(
      "n2kModelImputed",
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        result_datasource_id = dots$result_datasource_id,
        scheme_id = dots$scheme_id,
        species_group_id = dots$species_group_id,
        location_group_id = dots$location_group_id,
        model_type = dots$model_type,
        formula = dots$formula,
        first_imported_year = dots$first_imported_year,
        last_imported_year = dots$last_imported_year,
        duration = dots$duration,
        last_analysed_year = dots$last_analysed_year,
        analysis_date = dots$analysis_date,
        seed = dots$seed,
        status = dots$status,
        analysis_version = version@AnalysisVersion$fingerprint,
        file_fingerprint = file_fingerprint,
        status_fingerprint = status_fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(dots$formula)),
      AnalysisRelation = analysis_relation,
      Function = dots$model_fun,
      Package = dots$package,
      Filter = dots$filter,
      Mutate = dots$mutate,
      ModelArgs = dots$model_args,
      PrepareModelArgs = dots$prepare_model_args,
      Extractor = dots$extractor,
      ExtractorArgs = dots$extractor_args,
      AggregatedImputed = NULL,
      Results = NULL
    )
  }
)
