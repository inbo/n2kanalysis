#' Create a n2kModelImputed object
#' @inheritParams n2k_inla
#' @template analysis-metadata
#' @details
#' - `model_fun`: The `model_fun` argument of
#' \code{\link[multimput]{model_impute}}.
#' - `package`: A character vector of package names which must be loaded for
#' \code{model_fun}.
#' - `model_args`: An optional list for the `model_args` argument of
#' \code{\link[multimput]{model_impute}}.
#' - `extractor`: An optional list for the `extractor` argument of
#' \code{\link[multimput]{model_impute}}.
#' - `extractor_args`: An optional list for the `extractor_args` argument of
#' \code{\link[multimput]{model_impute}}.
#' - `filter`: An optional list for the `filter` argument of
#'  \code{\link[multimput]{model_impute}}.
#' - `mutate`: An optional list for the `mutate`` argument of
#' \code{\link[multimput]{model_impute}}.
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

#' @description A new n2kModelImputed model.
#' @rdname n2k_model_imputed
#' @aliases n2k_model_imputed,n2kModelImputed-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2kModelImputed_class.R
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
    } else {
      assert_that(is.count(dots$seed))
      dots$seed <- as.integer(dots$seed)
    }
    assert_that(is.string(dots$result_datasource_id))
    assert_that(is.string(dots$scheme_id))
    assert_that(is.string(dots$species_group_id))
    assert_that(is.string(dots$location_group_id))
    assert_that(is.string(dots$model_type))
    assert_that(is.string(dots$formula))
    assert_that(is.count(dots$first_imported_year))
    dots$first_imported_year <- as.integer(dots$first_imported_year)
    assert_that(is.count(dots$last_imported_year))
    dots$last_imported_year <- as.integer(dots$last_imported_year)
    if (is.null(dots$duration)) {
      dots$duration <- dots$last_imported_year - dots$first_imported_year + 1L
    } else {
      assert_that(is.count(dots$duration))
      dots$duration <- as.integer(dots$duration)
    }
    if (is.null(dots$last_analysed_year)) {
      dots$last_analysed_year <- dots$last_imported_year
    } else {
      assert_that(is.count(dots$last_analysed_year))
      dots$last_analysed_year <- as.integer(dots$last_analysed_year)
    }
    assert_that(is.time(dots$analysis_date))
    if (is.null(dots$filter)) {
      dots$filter <- list()
    } else {
      assert_that(is.list(dots$filter))
    }
    if (is.null(dots$mutate)) {
      dots$mutate <- list()
    } else {
      assert_that(is.list(dots$mutate))
    }
    assert_that(is.function(dots$model_fun))
    assert_that(is.function(dots$extractor))
    if (is.null(dots$model_args)) {
      dots$model_args <- list()
    } else {
      assert_that(is.list(dots$model_args))
    }
    if (is.null(dots$prepare_model_args)) {
      dots$prepare_model_args <- list()
    } else {
      assert_that(is.list(dots$prepare_model_args),
                  length(dots$prepare_model_args) <= 1)
      if (length(dots$prepare_model_args)) {
        assert_that(is.function(dots$prepare_model_args[[1]]))
      }
    }
    if (is.null(dots$extractor_args)) {
      dots$extractor_args <- list()
    } else {
      assert_that(is.list(dots$extractor_args))
    }
    if (is.null(dots$package)) {
      dots$package <- character(0)
    } else {
      assert_that(is.character(dots$package))
    }
    assert_that(is.string(dots$parent))

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
      if (is.null(dots$parent_status)) {
        dots$parent_status <- "waiting"
      }
      dots$parent_statusfingerprint <- sha1(dots$parent_status)
    } else {
      if (is.null(dots[["parent_status"]])) {
        stop(
"'parent_status' is required when 'parent_statusfingerprint' is provided"
        )
      }
    }
    analysis_relation <- data_frame(
      Analysis = file_fingerprint,
      ParentAnalysis = dots$parent,
      ParentStatusFingerprint = dots$parent_statusfingerprint,
      ParentStatus = dots$parent_status,
      stringsAsFactors = FALSE
    )
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, dots$status, version@AnalysisVersion$Fingerprint,
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
        ResultDatasourceID = dots$result_datasource_id,
        SchemeID = dots$scheme_id,
        SpeciesGroupID = dots$species_group_id,
        LocationGroupID = dots$location_group_id,
        ModelType = dots$model_type,
        Formula = dots$formula,
        FirstImportedYear = dots$first_imported_year,
        LastImportedYear = dots$last_imported_year,
        Duration = dots$duration,
        LastAnalysedYear = dots$last_analysed_year,
        AnalysisDate = dots$analysis_date,
        Seed = dots$seed,
        Status = dots$status,
        AnalysisVersion = version@AnalysisVersion$Fingerprint,
        FileFingerprint = file_fingerprint,
        StatusFingerprint = status_fingerprint,
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
