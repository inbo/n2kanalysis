#' Create a n2kInlaComparison object
#' @param parent_status A `data.frame` with columns
#' `parent_analysis` (the file fingerprint of the parent),
#' `parentstatus_fingerprint` (the status fingerprint of the parent),
#' and `parent_status` (the status of the parent).
#' @param ... other arguments
#' @name n2k_inla_comparison
#' @rdname n2k_inla_comparison
#' @exportMethod n2k_inla_comparison
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_inla_comparison",
  def = function(
    parent_status, ...
  ) {
    standardGeneric("n2k_inla_comparison") # nocov
  }
)

#' @description A new `n2kInlaComparison` model is created when `parent` is a
#' `character`.
#' @rdname n2k_inla_comparison
#' @aliases n2k_inla_comparison,n2kInlaComparison-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom assertthat assert_that noNA is.count is.string
#' @importFrom dplyr %>% select arrange
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_inla_comparison_class.R
#' @param result_datasource_id A string identifying the datasource.
#' @param status A single character indicating the status of the model.
#' Defaults to `"waiting"`.
#' @param scheme_id A single integer holding the id of the scheme.
#' @param species_group_id A string identifying the species group.
#' @param location_group_id A string identifying the location group.
#' @param model_type The type of the models.
#' Must start with `"inla comparison:"`.
#' @param formula A single character identifying the comparison.
#' @param first_imported_year Oldest year considered in the data.
#' @param last_imported_year Most recent year considered in the data.
#' @param duration The width of the moving window.
#' Defaults to the `last_imported_year - first_imported_year + 1`.
#' @param last_analysed_year Most recent year in the window.
#' Defaults to `last_imported_year`.
#' @param analysis_date A POSIXct date indicating the date that the dataset was
#' imported.
#' @param seed A single integer used as a seed for all calculations.
#' A random seed will be inserted when missing.
setMethod(
  f = "n2k_inla_comparison",
  signature = signature(parent_status = "data.frame"),
  definition = function(
    parent_status, status = "waiting", result_datasource_id, scheme_id,
    formula, species_group_id, location_group_id, model_type,
    first_imported_year, last_imported_year, duration, last_analysed_year,
    analysis_date, ..., seed
  ) {
    assert_that(is.string(status))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    } else {
      assert_that(is.count(seed))
      seed <- as.integer(seed)
    }
    assert_that(is.string(result_datasource_id))
    assert_that(is.string(scheme_id))
    assert_that(is.string(species_group_id))
    assert_that(is.string(location_group_id))
    assert_that(is.string(model_type))
    assert_that(is.string(formula))
    assert_that(is.count(first_imported_year))
    first_imported_year <- as.integer(first_imported_year)
    assert_that(is.count(last_imported_year))
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
    assert_that(
      has_name(parent_status, "parent_analysis"),
      has_name(parent_status, "parentstatus_fingerprint"),
      has_name(parent_status, "parent_status"),
      nrow(parent_status) > 1
    )
    parent_status <- parent_status %>%
      arrange(.data$parent_analysis)
    file_fingerprint <- sha1(
      list(
        result_datasource_id,
        scheme_id, species_group_id, location_group_id,
        model_type, formula, first_imported_year,
        last_imported_year, duration, last_analysed_year,
        format(analysis_date, tz = "UTC"), seed,
        parent_status$parent_analysis
      )
    )

    parent_status$analysis <- file_fingerprint
    parent_status <- parent_status %>%
      select(
        "analysis", "parent_analysis", "parentstatus_fingerprint",
        "parent_status"
      )
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, status, NULL,
        version@AnalysisVersion$fingerprint, version@AnalysisVersion,
        version@RPackage, version@AnalysisVersionRPackage, parent_status
      ),
      digits = 6L
    )

    new(
      "n2kInlaComparison",
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
      WAIC = NULL
    )
  }
)
