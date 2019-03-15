#' Create a n2kInlaComparison object
#' @param parent.status a `data.frame` with columns `ParentAnalysis` (the file fingerprint of the parent), `ParentStatusFingerprint` (the status fingerprint of the parent), and `ParentStatus` (the status of the parent)
#' @param ... other arguments
#' @name n2k_inla_comparison
#' @rdname n2k_inla_comparison
#' @exportMethod n2k_inla_comparison
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_inla_comparison",
  def = function(
    parent.status, ...
  ){
    standardGeneric("n2k_inla_comparison") # nocov
  }
)

#' @description A new n2kInlaComparison model is created when \code{parent} is a character
#' @rdname n2k_inla_comparison
#' @aliases n2k_inla_comparison,n2kInlaComparison-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom assertthat assert_that noNA is.count is.string
#' @importFrom dplyr %>% select_ arrange_
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2kInlaComparison_class.R
#' @param result.datasource.id the id of the results datasource
#' @param status a single character indicating the status of the model. Defaults to `"waiting"`.
#' @param scheme.id a single integer holding the id of the scheme.
#' @param species.group.id a single integer identifing the species group
#' @param location.group.id a single integer identifing the location group
#' @param model.type the type of the models. Must start with `"inla comparison:"`
#' @param formula a single character identifying the comparison
#' @param first.imported.year Oldest year considered in the data
#' @param last.imported.year Most recent year considered in the data
#' @param duration The width of the moving window. Defaults to the last.imported.year - first.imported.year + 1
#' @param last.analysed.year Most recent year in the window. Defaults to `last.imported.year`
#' @param analysis.date A POSIXct date indicating the date that the dataset was imported
#' @param seed a single integer used as a seed for all calculations. A random seed will be inserted when missing.
setMethod(
  f = "n2k_inla_comparison",
  signature = signature(parent.status = "data.frame"),
  definition = function(
    parent.status, status = "waiting", result.datasource.id, scheme.id,
    formula, species.group.id, location.group.id, model.type,
    first.imported.year, last.imported.year, duration, last.analysed.year,
    analysis.date, ..., seed
  ){
    assert_that(is.string(status))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    } else {
      assert_that(is.count(seed))
      seed <- as.integer(seed)
    }
    assert_that(is.string(result.datasource.id))
    assert_that(is.string(scheme.id))
    assert_that(is.string(species.group.id))
    assert_that(is.string(location.group.id))
    assert_that(is.string(model.type))
    assert_that(is.string(formula))
    assert_that(is.count(first.imported.year))
    first.imported.year <- as.integer(first.imported.year)
    assert_that(is.count(last.imported.year))
    last.imported.year <- as.integer(last.imported.year)
    if (missing(duration)) {
      duration <- last.imported.year - first.imported.year + 1L
    } else {
      assert_that(is.count(duration))
      duration <- as.integer(duration)
    }
    if (missing(last.analysed.year)) {
      last.analysed.year <- last.imported.year
    } else {
      assert_that(is.count(last.analysed.year))
      last.analysed.year <- as.integer(last.analysed.year)
    }
    assert_that(is.time(analysis.date))
    assert_that(
      has_name(parent.status, "ParentAnalysis"),
      has_name(parent.status, "ParentStatusFingerprint"),
      has_name(parent.status, "ParentStatus"),
      nrow(parent.status) > 1
    )
    parent.status <- parent.status %>%
      arrange(.data$ParentAnalysis)
    file.fingerprint <- sha1(
      list(
        result.datasource.id,
        scheme.id, species.group.id, location.group.id,
        model.type, formula, first.imported.year,
        last.imported.year, duration, last.analysed.year,
        format(analysis.date, tz = "UTC"), seed,
        parent.status$ParentAnalysis
      )
    )

    parent.status$Analysis <- file.fingerprint
    parent.status <- parent.status %>%
      select(
        "Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus"
      )
    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- sha1(
      list(
        file.fingerprint, status, NULL,
        version@AnalysisVersion$Fingerprint, version@AnalysisVersion,
        version@RPackage, version@AnalysisVersionRPackage, parent.status
      ),
      digits = 6L
    )

    new(
      "n2kInlaComparison",
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        ResultDatasourceID = result.datasource.id,
        SchemeID = scheme.id,
        SpeciesGroupID = species.group.id,
        LocationGroupID = location.group.id,
        ModelType = model.type,
        Formula = formula,
        FirstImportedYear = first.imported.year,
        LastImportedYear = last.imported.year,
        Duration = duration,
        LastAnalysedYear = last.analysed.year,
        AnalysisDate = analysis.date,
        Seed = seed,
        Status = status,
        AnalysisVersion = version@AnalysisVersion$Fingerprint,
        FileFingerprint = file.fingerprint,
        StatusFingerprint = status.fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = parent.status,
      WAIC = NULL
    )
  }
)
