#' Create a n2kAggregate object
#' @inheritParams n2k_glmer_poisson
#' @details
#'   \describe{
#'    \item{\code{scheme.id}}{a single integer holding the id of the scheme.}
#'    \item{\code{species.group.id}}{a single integer identifing the species group}
#'    \item{\code{location.group.id}}{a single integer identifing the location group}
#'    \item{\code{model.type}}{a single character identifying the type of model to fit to the data}
#'    \item{\code{formula}}{a single character holding the model formula}
#'    \item{\code{first.imported.year}}{Oldest year considered in the data}
#'    \item{\code{last.imported.year}}{Most recent year considered in the data}
#'    \item{\code{duration}}{The width of the moving window. Defaults to the last.imported.year - first.imported.year + 1}
#'    \item{\code{last.analysed.year}}{Most recent year in the window. Defaults to \code{last.imported.year}}
#'    \item{\code{analysis.date}}{A POSIXct date indicating the date that the dataset was imported}
#'    \item{\code{seed}}{a single integer used as a seed for all calculations. A random seed will be inserted when missing.}
#'    \item{\code{fun}}{the function to apply when aggregating}
#'    \item{\code{filter}}{an option list for the \code{filter} argument of \code{\link[multimput]{aggregate_impute}}}
#'    \item{\code{join}}{an option list for the \code{join} argument of \code{\link[multimput]{aggregate_impute}}}
#'   }
#' @name n2k_aggregate
#' @rdname n2k_aggregate
#' @exportMethod n2k_aggregate
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_aggregate",
  def = function(
    ...
  ){
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
#' @include n2kAggregate_class.R
setMethod(
  f = "n2k_aggregate",
  signature = signature("ANY"),
  definition = function(
    ...
  ){
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

    file.fingerprint <- sha1(
      list(
        dots$scheme.id, dots$species.group.id, dots$location.group.id,
        dots$model.type, dots$formula, dots$first.imported.year,
        dots$last.imported.year, dots$duration, dots$last.analysed.year,
        dots$analysis.date, dots$seed, dots$parent,
        dots$fun, dots$filter, dots$join
      )
    )

    if (is.null(dots$parent.statusfingerprint)) {
      if (is.null(dots$parent.status)) {
        dots$parent.status <- "waiting"
      }
      dots$parent.statusfingerprint <- sha1(dots$parent.status)
    } else {
      if (is.null(dots[["parent.status"]])) {
        stop(
"'parent.status' is required when 'parent.status.fingerprint' is provided"
        )
      }
    }
    analysis.relation <- data.frame(
      Analysis = file.fingerprint,
      ParentAnalysis = dots$parent,
      ParentStatusFingerprint = dots$parent.statusfingerprint,
      ParentStatus = dots$parent.status,
      stringsAsFactors = FALSE
    )
    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- sha1(
      list(
        file.fingerprint, dots$status, version@AnalysisVersion$Fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, analysis.relation, NULL, NULL
      ),
      digits = 6L
    )

    new(
      "n2kAggregate",
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
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
      Function = dots$fun,
      Filter = dots$filter,
      Join = dots$join,
      RawImputed = NULL,
      AggregatedImputed = NULL
    )
  }
)
