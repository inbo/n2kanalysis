#' Create an `n2kImport` object
#' @inheritParams n2k_inla
#' @template analysis_metadata
#' @details
#' - `formula`: a string holding the model formula.
#' - `dataset`: A `data.frame` with `filename`, `fingerprint` and `import_date`.
#' @name n2k_import
#' @rdname n2k_import
#' @exportMethod n2k_import
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_import",
  def = function(
    ...
  ) {
    standardGeneric("n2k_import") # nocov
  }
)

#' @description A new `n2kImport` model.
#' @rdname n2k_import
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_import_class.R
setMethod(
  f = "n2k_import",
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
    assert_that(inherits(dots$dataset, "data.frame"))

    file_fingerprint <- sha1(
      list(
        dots$result_datasource_id,
        dots$scheme_id, dots$species_group_id, dots$location_group_id,
        dots$model_type, dots$formula, dots$first_imported_year,
        dots$last_imported_year, dots$duration, dots$last_analysed_year,
        format(dots$analysis_date, tz = "UTC"), dots$seed, character(0)
      ),
      environment = FALSE
    )

    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, dots$status, version@AnalysisVersion$fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, dots$dataset
      ),
      digits = 6L
    )

    new(
      "n2kImport",
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
      Dataset = dots$dataset
    )
  }
)
