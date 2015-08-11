#' Create a n2kLrtGlmer object
#' @param parent the file fingerprint of the paretnt
#' @param ... other arguments. See below
#' @details
#'   \describe{
#'    \item{\code{status}}{a single character indicating the status of the model. Defaults to 'waiting' when \code{parent} is a character.}
#'    \item{\code{scheme.id}}{a single integer holding the id of the scheme.}
#'    \item{\code{species.group.id}}{a single integer identifing the species group}
#'    \item{\code{location.group.id}}{a single integer identifing the location group}
#'    \item{\code{formula}}{a single character identifying the comparison}
#'    \item{\code{first.imported.year}}{Oldest year considered in the data}
#'    \item{\code{last.imported.year}}{Most recent year considered in the data}
#'    \item{\code{duration}}{The width of the moving window. Defaults to the last.imported.year - first.imported.year + 1}
#'    \item{\code{last.analysed.year}}{Most recent year in the window. Defaults to \code{last.imported.year}}
#'    \item{\code{analysis.date}}{A POSIXct date indicating the date that the dataset was imported}
#'    \item{\code{seed}}{a single integer used as a seed for all calculations. A random seed will be inserted when missing.}
#'   }
#' @name n2k_lrt_glmer
#' @rdname n2k_lrt_glmer
#' @exportMethod n2k_lrt_glmer
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_lrt_glmer", 
  def = function(
    parent, ...
  ){
    standard.generic("n2k_lrt_glmer")
  }
)

#' @description A new n2kLrtGlmer model is created when \code{parent} is a character
#' @rdname n2k_lrt_glmer
#' @aliases n2k_lrt_glmer,n2kLrtGlmer-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom assertthat assert_that is.count is.string
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "n2k_lrt_glmer", 
  signature = signature(parent = "character"),
  definition = function(
    parent, ...
  ){
    dots <- list(...)
    assert_that(is.string(parent))
    assert_that(is.string(dots$parent.0))
    #set the defaults for missing arguments in dots
    if (is.null(dots$status)) {
      dots$status <- "waiting"
    }
    assert_that(is.count(dots$seed))
    dots$seed <- is.integer(dots$seed)
    assert_that(is.count(dots$scheme.id))
    dots$scheme.id <- as.integer(dots$scheme.id)
    assert_that(is.count(dots$species.group.id))
    dots$species.group.id <- as.integer(dots$species.group.id)
    assert_that(is.count(dots$location.group.id))
    dots$location.group.id <- as.integer(dots$location.group.id)
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
    check_dataframe_variable(
      df = dots$parent.status,
      name = "parent.status",
      variable = c("ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
    )
    dots$parent.status <- dots$parent.status[
      order(dots$parent.status$ParentAnalysis),
    ]
    file.fingerprint <- get_sha1(
      list(
        dots$scheme.id, dots$species.group.id, dots$location.group.id, 
        dots$model.type, dots$formula, dots$first.imported.year, dots$last.imported.year,
        dots$duration, dots$last.analysed.year, dots$analysis.date, dots$seed, 
        dots$parent.0, dots$parent.status$ParentAnalysis
      )
    )
    dots$parent.status$Analysis <- file.fingerprint
    dots$parent.status <- dots$parent.status[
      ,
      c("Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
    ]
    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- get_sha1(
      list(
        file.fingerprint, dots$status, NULL, NULL, NULL, 
        version@AnalysisVersion$Fingerprint, version@AnalysisVersion,
        version@RPackage, version@AnalysisVersionRPackage, dots$parent.status
      )
    )
    
    new(
      "n2kLrtGlmer",
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
      AnalysisRelation = dots$parent.status,
      Parent0 = dots$parent.0,
      Model = NULL,
      Model0 = NULL,
      Anova = NULL
    )
  }
)
