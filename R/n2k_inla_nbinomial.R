#' Create a n2kInlaNbinomial object
#' @inheritParams n2k_glmer_poisson
#' @details
#'   \describe{
#'    \item{\code{scheme.id}}{a single integer holding the id of the scheme.}
#'    \item{\code{species.group.id}}{a single integer identifing the species group}
#'    \item{\code{location.group.id}}{a single integer identifing the location group}
#'    \item{\code{model.type}}{a single character identifying the type of model to fit to the data}
#'    \item{\code{covariate}}{a single character holding the right hand side of the model formula}
#'    \item{\code{first.imported.year}}{Oldest year considered in the data}
#'    \item{\code{last.imported.year}}{Most recent year considered in the data}
#'    \item{\code{duration}}{The width of the moving window. Defaults to the last.imported.year - first.imported.year + 1}
#'    \item{\code{last.analysed.year}}{Most recent year in the window. Defaults to \code{last.imported.year}}
#'    \item{\code{analysis.date}}{A POSIXct date indicating the date that the dataset was imported}
#'    \item{\code{seed}}{a single integer used as a seed for all calculations. A random seed will be inserted when missing.}
#'   }
#' @name n2k_inla_nbinomial
#' @rdname n2k_inla_nbinomial
#' @exportMethod n2k_inla_nbinomial
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_inla_nbinomial", 
  def = function(
    data, ..., model.fit
  ){
    standard.generic("n2k_inla_nbinomial")
  }
)

#' @description A new n2kInlaNbinomial model is created when \code{data} is a data.frame.
#' @rdname n2k_inla_nbinomial
#' @aliases n2k_inla_nbinomial,n2kInlaNbinomial-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "n2k_inla_nbinomial", 
  signature = signature(data = "data.frame"),
  definition = function(
    data, ..., model.fit
  ){
    dots <- list(...)
    #set the defaults for missing arguments in dots
    if (is.null(dots$status)) {
      dots$status <- "new"
    }
    if (is.null(dots$seed)) {
      dots$seed <- sample(.Machine$integer.max, 1)
    } else {
      dots$seed <- check_single_strictly_positive_integer(dots$seed, name = "seed")
    }
    dots$scheme.id <- check_single_strictly_positive_integer(
      dots$scheme.id, 
      name = "scheme.id"
    )
    dots$species.group.id <- check_single_strictly_positive_integer(
      dots$species.group.id, 
      name = "species.group.id"
    )
    dots$location.group.id <- check_single_strictly_positive_integer(
      dots$location.group.id, 
      name = "location.group.id"
    )
    dots$model.type <- check_single_character(dots$model.type, name = "model.type")
    dots$formula <- check_single_character(dots$formula, name = "formula")
    dots$first.imported.year <- check_single_strictly_positive_integer(
      dots$first.imported.year, 
      name = "first.imported.year"
    )
    dots$last.imported.year <- check_single_strictly_positive_integer(
      dots$last.imported.year, 
      name = "last.imported.year"
    )
    if (is.null(dots$duration)) {
      dots$duration <- dots$last.imported.year - dots$first.imported.year + 1L
    } else {
      dots$duration <- check_single_strictly_positive_integer(
        dots$duration, 
        name = "duration"
      )
    }
    if (is.null(dots$last.analysed.year)) {
      dots$last.analysed.year <- dots$last.imported.year
    } else {
      dots$last.analysed.year <- check_single_strictly_positive_integer(
        dots$last.analysed.year, 
        name = "last.analysed.year"
      )
    }
    dots$analysis.date <- check_single_posix(
      dots$analysis.date, 
      name = "analysis.date", 
      past = TRUE
    )
    if (is.null(dots$parent)) {
      dots$parent <- character(0)
    }
    file.fingerprint <- get_sha1(
      list(
        data, dots$scheme.id, dots$species.group.id, dots$location.group.id, 
        dots$model.type, dots$covariate, dots$first.imported.year, 
        dots$last.imported.year, dots$duration, dots$last.analysed.year, 
        dots$analysis.date, dots$seed, dots$parent
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
      dots$parent <- check_single_character(dots$parent, name = "parent")
      if (is.null(dots$parent.status.fingerprint)) {
        if (is.null(dots$parent.status)) {
          dots$parent.status <- "converged"
        }
        dots$parent.statusfingerprint <- get_sha1(dots$parent.status)
      } else {
        if (is.null(dots$parent.status)) {
          stop("'parent.status' is required when 'parent.status.fingerprint' is provided")
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
    status.fingerprint <- get_sha1(
      list(
        file.fingerprint, dots$status, NULL, version@AnalysisVersion$Fingerprint, 
        version@AnalysisVersion, version@RPackage, version@AnalysisVersionRPackage,
        analysis.relation
      )
    )

    new(
      "n2kInlaNbinomial",
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
      Data = data,
      Model = NULL
    )
  }
)

#' @description In case \code{data} a n2kInlaNbinomial object is, then only the model and status are updated. All other slots are unaffected.
#' @rdname n2k_inla_nbinomial
#' @aliases n2k_inla_nbinomial,n2kInlaNbinomial-methods
#' @importFrom methods setMethod validObject
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "n2k_inla_nbinomial", 
  signature = signature(data = "n2kInlaNbinomial", model.fit = "inla"),
  definition = function(
    data, ..., model.fit
  ){
    dots <- list(...)
    data@Model <- model.fit
    data@AnalysisMetadata$Status <- dots$status
    version <- get_analysis_version(sessionInfo())
    new.version <- union(data, version)
    data@AnalysisVersion <- new.version$Union@AnalysisVersion
    data@RPackage <- new.version$Union@RPackage
    data@AnalysisVersionRPackage <- new.version$Union@AnalysisVersionRPackage
    data@AnalysisMetadata$AnalysisVersion <- new.version$UnionFingerprint
    data@AnalysisMetadata$StatusFingerprint <- get_sha1(
      list(
        data@AnalysisMetadata$FileFingerprint, data@AnalysisMetadata$Status, 
        data@Model, data@AnalysisMetadata$AnalysisVersion, 
        data@AnalysisVersion, data@RPackage, data@AnalysisVersionRPackage, 
        data@AnalysisRelation
      )
    )
    
    validObject(data)
    return(data)
  }
)
