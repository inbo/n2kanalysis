#' Create a n2kGlmerPoisson object
#' @param data a data.frame with the data to analyse
#' @param model.fit The fitted model
#' @param ... other arguments. See below
#' @template analysis-metadata
#' @details
#' - `status`: a single character indicating the status of the model.
#' Defaults to 'new' when `data` is a `data.frame`.
#' - `covariate`: a single character holding the right hand side of the model
#' formula.
#' - `parent`: the file fingerprint of the import.
#' @name n2k_glmer_poisson
#' @rdname n2k_glmer_poisson
#' @exportMethod n2k_glmer_poisson
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_glmer_poisson",
  def = function(
    data, ..., model.fit
  ) {
    standardGeneric("n2k_glmer_poisson") # nocov
  }
)

#' @description A new `n2kGlmerPoisson` model is created when `data` is a
#' `data.frame`.
#' @rdname n2k_glmer_poisson
#' @aliases n2k_glmer_poisson,n2kGlmerPoisson-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time noNA
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @importFrom n2khelper is.chartor
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "n2k_glmer_poisson",
  signature = signature(data = "data.frame"),
  definition = function(
    data, ..., model.fit
  ) {
    dots <- list(...)
    #set the defaults for missing arguments in dots
    if (is.null(dots$status)) {
      dots$status <- "new"
    } else {
      assert_that(is.string(dots$status))
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
    if (is.null(dots$parent)) {
      dots$parent <- character(0)
    } else {
      assert_that(is.chartor(dots$parent))
      assert_that(noNA(dots$parent))
    }
    file.fingerprint <- sha1(
      list(
        data, dots$result.datasource.id, dots$scheme.id, dots$species.group.id,
        dots$location.group.id, dots$model.type, dots$formula,
        dots$first.imported.year, dots$last.imported.year, dots$duration,
        dots$last.analysed.year, format(dots$analysis.date, tz = "UTC"),
        dots$seed, dots$parent
      )
    )
    if (length(dots$parent) == 0) {
      analysis.relation <- data.frame(
        Analysis = character(0),
        ParentAnalysis = character(0),
        ParentStatusFingerprint = character(0),
        ParentStatus = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      if (is.null(dots$parent.status.fingerprint)) {
        if (is.null(dots$parent.status)) {
          dots$parent.status <- "converged"
        }
        dots$parent.status.fingerprint <- sha1(dots$parent.status)
      } else {
        if (is.null(dots$parent.status)) {
          stop(
"'parent.status' is required when 'parent.status.fingerprint' is provided"
          )
        }
      }
      analysis.relation <- data.frame(
        Analysis = file.fingerprint,
        ParentAnalysis = dots$parent,
        ParentStatusFingerprint = dots$parent.status.fingerprint,
        ParentStatus = dots$parent.status,
        stringsAsFactors = FALSE
      )
    }
    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- sha1(
      list(
        file.fingerprint, dots$status, NULL,
        version@AnalysisVersion$Fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, analysis.relation
      ),
      digits = 6L
    )

    new(
      "n2kGlmerPoisson",
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
      AnalysisRelation = analysis.relation,
      Data = data,
      Model = NULL
    )
  }
)

#' @description In case `data` is an `n2kGlmerPoisson` object, only the model
#' and status are updated.
#' All other slots are unaffected.
#' @rdname n2k_glmer_poisson
#' @aliases n2k_glmer_poisson,my_lmer-methods
#' @importFrom methods setMethod validObject new
#' @importFrom utils sessionInfo
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "n2k_glmer_poisson",
  signature = signature(data = "n2kGlmerPoisson", model.fit = "glmerMod"),
  definition = function(
    data, ..., model.fit
  ) {
    dots <- list(...)

    data@Model <- model.fit

    version <- get_analysis_version(sessionInfo())
    new.version <- union(data, version)
    data@AnalysisVersion <- new.version$Union@AnalysisVersion
    data@RPackage <- new.version$Union@RPackage
    data@AnalysisVersionRPackage <- new.version$Union@AnalysisVersionRPackage

    data@AnalysisMetadata$AnalysisVersion <- new.version$UnionFingerprint

    status(data) <- dots$status
    return(data)
  }
)
