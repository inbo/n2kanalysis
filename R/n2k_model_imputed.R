#' Create a n2kModelImputed object
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
#'    \item{\code{model.fun}}{the \code{model.fun} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{package}}{a character vector of package names which must be loaded for \code{model.fun}}
#'    \item{\code{model.args}}{an option list for the \code{model.args} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{extractor}}{an option list for the \code{extractor} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{extractor.args}}{an option list for the \code{extractor.args} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{filter}}{an option list for the \code{filter} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{mutate}}{an option list for the \code{mutate} argument of \code{\link[multimput]{model_impute}}}
#'   }
#' @name n2k_model_imputed
#' @rdname n2k_model_imputed
#' @exportMethod n2k_model_imputed
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_model_imputed",
  def = function(
    ...
  ){
    standardGeneric("n2k_model_imputed") # nocov
  }
)

#' @description A new n2kModelImputed model.
#' @rdname n2k_model_imputed
#' @aliases n2k_model_imputed,n2kModelImputed-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2kModelImputed_class.R
setMethod(
  f = "n2k_model_imputed",
  signature = signature("ANY"),
  definition = function(
    ...
  ){
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
    if (is.null(dots$filter)) {
      dots$filter <- list()
    } else {
      assert_that(is.list(dots$filter))
    }
    if (is.null(dots$mutate)) {
      dots$mutate <- list()
    } else {
      assert_that(is.list(dots$mutate))
    }
    assert_that(is.function(dots$model.fun))
    assert_that(is.function(dots$extractor))
    if (is.null(dots$model.args)) {
      dots$model.args <- list()
    } else {
      assert_that(is.list(dots$model.args))
    }
    if (is.null(dots$prepare.model.args)) {
      dots$prepare.model.args <- list()
    } else {
      assert_that(is.list(dots$prepare.model.args),
                  length(dots$prepare.model.args) <= 1)
      if (length(dots$prepare.model.args)) {
        assert_that(is.function(dots$prepare.model.args[[1]]))
      }
    }
    if (is.null(dots$extractor.args)) {
      dots$extractor.args <- list()
    } else {
      assert_that(is.list(dots$extractor.args))
    }
    if (is.null(dots$package)) {
      dots$package <- character(0)
    } else {
      assert_that(is.character(dots$package))
    }
    assert_that(is.string(dots$parent))

    file.fingerprint <- sha1(
      list(
        dots$result.datasource.id,
        dots$scheme.id, dots$species.group.id, dots$location.group.id,
        dots$model.type, dots$formula, dots$first.imported.year,
        dots$last.imported.year, dots$duration, dots$last.analysed.year,
        format(dots$analysis.date, tz = "UTC"),
        dots$seed, dots$parent, dots$model.fun, dots$filter,
        dots$mutate, dots$model.args, dots$prepare.model.args, dots$extractor,
        dots$extractor.args, dots$package
      ),
      environment = FALSE
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
      "n2kModelImputed",
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
      Function = dots$model.fun,
      Package = dots$package,
      Filter = dots$filter,
      Mutate = dots$mutate,
      ModelArgs = dots$model.args,
      PrepareModelArgs = dots$prepare.model.args,
      Extractor = dots$extractor,
      ExtractorArgs = dots$extractor.args,
      AggregatedImputed = NULL,
      Results = NULL
    )
  }
)
