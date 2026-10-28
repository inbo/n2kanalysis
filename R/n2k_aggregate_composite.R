#' Create an `n2kAggregateComposite` object
#' @name n2k_aggregate_composite
#' @rdname n2k_aggregate_composite
#' @exportMethod n2k_aggregate_composite
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_aggregate_composite",
  def = function(
    parent_status,
    ...
  ) {
    standardGeneric("n2k_aggregate_composite") # nocov
  }
)

#' @description A new `n2kAggregateComposite` model is created when `parent` is
#' a `data.frame`.
#' @rdname n2k_aggregate_composite
#' @aliases n2k_aggregate_composite,n2kAggregateComposite-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom dplyr arrange select
#' @importFrom rlang .data
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_aggregate_composite_class.R
#' @inheritParams n2k_inla_comparison
#' @param fun A function that takes in a vector of values from the imputations
#' and returns a single value.
#' This function is used to combine the parent imputations into a single
#' composite imputation.
setMethod(
  f = "n2k_aggregate_composite",
  signature = signature(parent_status = "data.frame"),
  definition = function(
    parent_status,
    status = "waiting",
    result_datasource_id,
    scheme_id,
    formula,
    species_group_id,
    location_group_id,
    model_type,
    first_imported_year,
    last_imported_year,
    duration,
    last_analysed_year,
    analysis_date,
    fun,
    ...,
    seed
  ) {
    assert_that(
      has_name(parent_status, "parent_analysis"),
      has_name(parent_status, "parentstatus_fingerprint"),
      has_name(parent_status, "parent_status")
    )
    assert_that(is.string(status))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    } else {
      assert_that(is.count(seed))
      seed <- as.integer(seed)
    }
    assert_that(
      is.string(result_datasource_id),
      is.string(scheme_id),
      is.string(species_group_id),
      is.string(location_group_id),
      is.string(model_type),
      is.string(formula),
      is.count(first_imported_year),
      is.count(last_imported_year)
    )
    first_imported_year <- as.integer(first_imported_year)
    last_imported_year <- as.integer(last_imported_year)
    if (missing(duration)) {
      duration <- last_imported_year - first_imported_year + 1L
    } else {
      assert_that(is.count(duration))
      duration <- as.integer(duration)
    }
    if (missing(last_analysed_year)) {
      last_analysed_year <- last_imported_year
    } else {
      assert_that(is.count(last_analysed_year))
      last_analysed_year <- as.integer(last_analysed_year)
    }
    assert_that(is.time(analysis_date))
    assert_that(inherits(fun, "function"))
    file_fingerprint <- sha1(
      list(
        result_datasource_id,
        scheme_id,
        species_group_id,
        location_group_id,
        model_type,
        formula,
        first_imported_year,
        last_imported_year,
        duration,
        last_analysed_year,
        format(analysis_date, tz = "UTC"),
        seed,
        parent_status$parent_analysis,
        formals(fun),
        as.character(body(fun))
      )
    )
    parent_status$analysis <- file_fingerprint
    parent_status <- parent_status |>
      select(
        "analysis",
        "parent_analysis",
        "parentstatus_fingerprint",
        "parent_status"
      )

    version <- get_analysis_version(sessionInfo())

    status_fingerprint <- sha1(
      list(
        file_fingerprint,
        status,
        version@AnalysisVersion$fingerprint,
        version@AnalysisVersion,
        version@RPackage,
        version@AnalysisVersionRPackage,
        parent_status,
        list(),
        NULL
      ),
      digits = 6L
    )

    new(
      "n2kAggregateComposite",
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        result_datasource_id = result_datasource_id,
        scheme_id = scheme_id,
        species_group_id = species_group_id,
        location_group_id = location_group_id,
        model_type = model_type,
        formula = formula,
        first_imported_year = first_imported_year,
        last_imported_year = last_imported_year,
        duration = duration,
        last_analysed_year = last_analysed_year,
        analysis_date = analysis_date,
        seed = seed,
        status = status,
        analysis_version = version@AnalysisVersion$fingerprint,
        file_fingerprint = file_fingerprint,
        status_fingerprint = status_fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = parent_status,
      Fun = fun,
      Input = list(),
      AggregatedImputed = NULL
    )
  }
)
