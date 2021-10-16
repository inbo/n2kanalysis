#' Create a n2kImport object
#' @inheritParams n2k_inla
#' @template analysis-metadata
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

#' @description A new n2kImport model.
#' @rdname n2k_import
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2kImport_class.R
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
    assert_that(is.string(dots$result.datasource.id))
    assert_that(is.string(dots$scheme.id))
    assert_that(is.string(dots$species.group.id))
    assert_that(is.string(dots$location.group.id))
    assert_that(is.string(dots$model.type))
    assert_that(is.string(dots$formula))
    assert_that(is.count(dots$first.imported.year))
    dots$first.imported.year <- as.integer(dots$first.imported.year)
    assert_that(is.count(dots$last.imported.year))
    dots$last.imported.year <- as.integer(dots$last.imported.year)
    if (is.null(dots$duration)) {
      dots$duration <- dots$last.imported.year - dots$first.imported.year + 1L
    } else {
      assert_that(is.count(dots$duration))
      dots$duration <- as.integer(dots$duration)
    }
    if (is.null(dots$last.analysed.year)) {
      dots$last.analysed.year <- dots$last.imported.year
    } else {
      assert_that(is.count(dots$last.analysed.year))
      dots$last.analysed.year <- as.integer(dots$last.analysed.year)
    }
    assert_that(is.time(dots$analysis.date))
    assert_that(inherits(dots$dataset, "data.frame"))

    file.fingerprint <- sha1(
      list(
        dots$result.datasource.id,
        dots$scheme.id, dots$species.group.id, dots$location.group.id,
        dots$model.type, dots$formula, dots$first.imported.year,
        dots$last.imported.year, dots$duration, dots$last.analysed.year,
        format(dots$analysis.date, tz = "UTC"), dots$seed, character(0)
      ),
      environment = FALSE
    )

    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- sha1(
      list(
        file.fingerprint, dots$status, version@AnalysisVersion$Fingerprint,
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
        ResultDatasourceID = dots$result.datasource.id,
        SchemeID = dots$scheme.id,
        SpeciesGroupID = dots$species.group.id,
        LocationGroupID = dots$location.group.id,
        ModelType = dots$model.type,
        Formula = dots$formula,
        FirstImportedYear = dots$first.imported.year,
        LastImportedYear = dots$last.imported.year,
        Duration = dots$duration,
        LastAnalysedYear = dots$last.analysed.year,
        AnalysisDate = dots$analysis.date,
        Seed = dots$seed,
        Status = dots$status,
        AnalysisVersion = version@AnalysisVersion$Fingerprint,
        FileFingerprint = file.fingerprint,
        StatusFingerprint = status.fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(dots$formula)),
      Dataset = dots$dataset
    )
  }
)
