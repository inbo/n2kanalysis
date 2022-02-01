#' Create a n2kComposite object
#' @name n2k_composite
#' @rdname n2k_composite
#' @exportMethod n2k_composite
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_composite",
  def = function(
    parent_status, ...
  ) {
    standardGeneric("n2k_composite") # nocov
  }
)

#' @description A new `n2kComposite` model is created when `parent` is a
#' `data.frame`.
#' @rdname n2k_composite
#' @aliases n2k_composite,n2kComposite-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom dplyr %>% arrange
#' @importFrom rlang .data
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_composite_class.R
#' @inheritParams n2k_inla_comparison
#' @param extractor a function to extract the relevant parameters from the model
setMethod(
  f = "n2k_composite",
  signature = signature(parent_status = "data.frame"),
  definition = function(
    parent_status, status = "waiting", result_datasource_id, scheme_id,
    formula, species_group_id, location_group_id, model_type,
    first_imported_year, last_imported_year, duration, last_analysed_year,
    analysis_date, extractor, ..., seed
  ) {
    assert_that(
      has_name(parent_status, "parent_analysis"),
      has_name(parent_status, "parentstatus_fingerprint"),
      has_name(parent_status, "parent_status")
    )
    parent_status <- parent_status %>%
      arrange(.data$parent_analysis)
    assert_that(is.string(status))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    } else {
      assert_that(is.count(seed))
      seed <- as.integer(seed)
    }
    assert_that(
      is.string(result_datasource_id), is.string(scheme_id),
      is.string(species_group_id), is.string(location_group_id),
      is.string(model_type), is.string(formula), is.count(first_imported_year),
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
    assert_that(inherits(extractor, "function"))
    file_fingerprint <- sha1(
      list(
        result_datasource_id, scheme_id, species_group_id, location_group_id,
        model_type, formula, first_imported_year, last_imported_year, duration,
        last_analysed_year, format(analysis_date, tz = "UTC"), seed,
        parent_status$parent_analysis, formals(extractor),
        as.character(body(extractor))
      )
    )
    parent_status$analysis <- file_fingerprint
    parent_status <- parent_status %>%
      select(
        "analysis", "parent_analysis", "parentstatus_fingerprint",
        "parent_status"
      )
    parameter <- data.frame(
      parent = character(0), value = character(0), estimate = numeric(0),
      variance = numeric(0), stringsAsFactors = FALSE
    )
    index <- data.frame(
      calue = character(0), estimate = numeric(0),
      lower_confidence_limit = numeric(0), upper_confidence_limit = numeric(0),
      stringsAsFactors = FALSE
    )

    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, status, parameter, index,
        version@AnalysisVersion$fingerprint, version@AnalysisVersion,
        version@RPackage, version@AnalysisVersionRPackage, parent_status
      ),
      digits = 6L
    )

    new(
      "n2kComposite",
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        result_datasource_id = result_datasource_id, scheme_id = scheme_id,
        species_group_id = species_group_id,
        location_group_id = location_group_id, model_type = model_type,
        formula = formula, first_imported_year = first_imported_year,
        last_imported_year = last_imported_year, duration = duration,
        last_analysed_year = last_analysed_year, analysis_date = analysis_date,
        seed = seed, status = status,
        analysis_version = version@AnalysisVersion$fingerprint,
        file_fingerprint = file_fingerprint,
        status_fingerprint = status_fingerprint, stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = parent_status,
      Extractor = extractor,
      Parameter = parameter,
      Index = index
    )
  }
)
