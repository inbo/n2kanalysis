#' Create a n2kAggregate object
#' @inheritParams n2k_inla
#' @template analysis_metadata
#' @details
#' - `fun`: The function to apply when aggregating.
#' - `filter`: An optional list for the \code{filter} argument of
#' \code{\link[multimput]{aggregate_impute}}.
#' - `join`: An optional list for the \code{join} argument of
#' \code{\link[multimput]{aggregate_impute}}.
#' @name n2k_aggregate
#' @rdname n2k_aggregate
#' @exportMethod n2k_aggregate
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_aggregate",
  def = function(
    ...
  ) {
    standardGeneric("n2k_aggregate") # nocov
  }
)

#' @description A new n2kAggregate model.
#' @rdname n2k_aggregate
#' @aliases n2k_aggregate,n2kAggregate-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_aggregate_class.R
setMethod(
  f = "n2k_aggregate",
  signature = signature("ANY"),
  definition = function(
    ...
  ) {
    dots <- list(...)
    #set the defaults for missing arguments in dots
    if (is.null(dots$status)) {
      dots$status <- "waiting"
    }
    if (is.null(dots$minimum)) {
      dots$minimum <- ""
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
    if (is.null(dots$join)) {
      dots$join <- list()
    } else {
      if (inherits(dots$join, "data.frame")) {
        dots$join <- list(dots$join)
      } else {
        assert_that(is.list(dots$join))
      }
    }
    assert_that(is.function(dots$fun))
    assert_that(is.string(dots$parent))

    file_fingerprint <- sha1(
      list(
        dots$result_datasource_id,
        dots$scheme_id, dots$species_group_id, dots$location_group_id,
        dots$model_type, dots$formula, dots$first_imported_year,
        dots$last_imported_year, dots$duration, dots$last_analysed_year,
        format(dots$analysis_date, tz = "UTC"), dots$seed, dots$parent,
        dots$fun, dots$filter, dots$join
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
    analysis_relation <- data.frame(
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
      "n2kAggregate",
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
      Function = dots$fun,
      Filter = dots$filter,
      Join = dots$join,
      RawImputed = NULL,
      AggregatedImputed = NULL
    )
  }
)
