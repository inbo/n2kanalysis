#' Create a n2kComposite object
#' @name n2k_composite
#' @rdname n2k_composite
#' @exportMethod n2k_composite
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_composite",
  def = function(
    parent.status, ...
  ){
    standardGeneric("n2k_composite") # nocov
  }
)

#' @description A new n2kComposite model is created when \code{parent} is a data.frame
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
#' @include n2kComposite_class.R
#' @inheritParams n2k_inla_comparison
#' @param extractor a function to extract the relevant parameters from the model
setMethod(
  f = "n2k_composite",
  signature = signature(parent.status = "data.frame"),
  definition = function(
    parent.status, status = "waiting", result.datasource.id, scheme.id,
    formula, species.group.id, location.group.id, model.type,
    first.imported.year, last.imported.year, duration, last.analysed.year,
    analysis.date, extractor, ..., seed
  ){
    assert_that(
      has_name(parent.status, "ParentAnalysis"),
      has_name(parent.status, "ParentStatusFingerprint"),
      has_name(parent.status, "ParentStatus")
    )
    parent.status <- parent.status %>%
      arrange(.data$ParentAnalysis)
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
    assert_that(inherits(extractor, "function"))
    file.fingerprint <- sha1(
      list(
        result.datasource.id,
        scheme.id, species.group.id, location.group.id,
        model.type, formula, first.imported.year,
        last.imported.year, duration, last.analysed.year,
        format(analysis.date, tz = "UTC"),
        seed, parent.status$ParentAnalysis,
        formals(extractor),
        as.character(body(extractor))
      )
    )
    parent.status$Analysis <- file.fingerprint
    parent.status <- parent.status %>%
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
    status.fingerprint <- sha1(
      list(
        file.fingerprint, status, parameter, index,
        version@AnalysisVersion$Fingerprint, version@AnalysisVersion,
        version@RPackage, version@AnalysisVersionRPackage, parent.status
      ),
      digits = 6L
    )

    new(
      "n2kComposite",
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
      Extractor = extractor,
      Parameter = parameter,
      Index = index
    )
  }
)
