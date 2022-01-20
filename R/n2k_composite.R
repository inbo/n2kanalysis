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
      has_name(parent_status, "ParentAnalysis"),
      has_name(parent_status, "ParentStatusFingerprint"),
      has_name(parent_status, "ParentStatus")
    )
    parent_status <- parent_status %>%
      arrange(.data$ParentAnalysis)
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
    assert_that(inherits(extractor, "function"))
    file_fingerprint <- sha1(
      list(
        result_datasource_id,
        scheme_id, species_group_id, location_group_id,
        model_type, formula, first_imported_year,
        last_imported_year, duration, last_analysed_year,
        format(analysis_date, tz = "UTC"),
        seed, parent_status$ParentAnalysis,
        formals(extractor),
        as.character(body(extractor))
      )
    )
    parent_status$Analysis <- file_fingerprint
    parent_status <- parent_status %>%
      select(
        "Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus"
      )
    parameter <- data.frame(
      Parent = character(0),
      Value = character(0),
      Estimate = numeric(0),
      Variance = numeric(0),
      stringsAsFactors = FALSE
    )
    index <- data.frame(
      Value = character(0),
      Estimate = numeric(0),
      LowerConfidenceLimit = numeric(0),
      UpperConfidenceLimit = numeric(0),
      stringsAsFactors = FALSE
    )

    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, status, parameter, index,
        version@AnalysisVersion$Fingerprint, version@AnalysisVersion,
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
        ResultDatasourceID = result_datasource_id,
        SchemeID = scheme_id,
        SpeciesGroupID = species_group_id,
        LocationGroupID = location_group_id,
        ModelType = model_type,
        Formula = formula,
        FirstImportedYear = first_imported_year,
        LastImportedYear = last_imported_year,
        Duration = duration,
        LastAnalysedYear = last_analysed_year,
        AnalysisDate = analysis_date,
        Seed = seed,
        Status = status,
        AnalysisVersion = version@AnalysisVersion$Fingerprint,
        FileFingerprint = file_fingerprint,
        StatusFingerprint = status_fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = parent_status,
      Extractor = extractor,
      Parameter = parameter,
      Index = index
    )
  }
)
