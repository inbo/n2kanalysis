#' Create a n2kComposite object
#' @param parent the file fingerprint of the parents
#' @param model.fit The fitted model
#' @param ... other arguments. See below
#' @details
#'   \describe{
#'    \item{\code{status}}{a single character indicating the status of the model. Defaults to 'waiting' when \code{parent} is a character.}
#'    \item{\code{scheme.id}}{a single integer holding the id of the scheme.}
#'    \item{\code{species.group.id}}{a single integer identifing the species group}
#'    \item{\code{location.group.id}}{a single integer identifing the location group}
#'    \item{\code{covariate}}{a single character identifying the comparison}
#'    \item{\code{first.imported.year}}{Oldest year considered in the data}
#'    \item{\code{last.imported.year}}{Most recent year considered in the data}
#'    \item{\code{duration}}{The width of the moving window. Defaults to the last.imported.year - first.imported.year + 1}
#'    \item{\code{last.analysed.year}}{Most recent year in the window. Defaults to \code{last.imported.year}}
#'    \item{\code{analysis.date}}{A POSIXct date indicating the date that the dataset was imported}
#'    \item{\code{seed}}{a single integer used as a seed for all calculations. A random seed will be inserted when missing.}
#'   }
#' @name n2k_composite
#' @rdname n2k_composite
#' @exportMethod n2k_composite
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_composite", 
  def = function(
    parent, ..., model.fit
  ){
    standard.generic("n2k_composite")
  }
)

#' @description A new n2kLrtGlmer model is created when \code{parent} is a character
#' @rdname n2k_composite
#' @aliases n2k_composite,n2kComposite-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "n2k_composite", 
  signature = signature(parent = "character"),
  definition = function(
    parent, ..., model.fit
  ){
    parent <- sort(parent)
    
    dots <- list(...)
    #set the defaults for missing arguments in dots
    if(is.null(dots$status)){
      dots$status <- "waiting"
    }
    dots$seed <- check_single_strictly_positive_integer(dots$seed, name = "seed")
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
    dots$covariate <- check_single_character(dots$covariate, name = "covariate")
    dots$first.imported.year <- check_single_strictly_positive_integer(
      dots$first.imported.year, 
      name = "first.imported.year"
    )
    dots$last.imported.year <- check_single_strictly_positive_integer(
      dots$last.imported.year, 
      name = "last.imported.year"
    )
    if(is.null(dots$duration)){
      dots$duration <- dots$last.imported.year - dots$first.imported.year + 1L
    } else {
      dots$duration <- check_single_strictly_positive_integer(
        dots$duration, 
        name = "duration"
      )
    }
    if(is.null(dots$last.analysed.year)){
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
    file.fingerprint <- digest(
      list(
        dots$scheme.id, dots$species.group.id, dots$location.group.id, 
        dots$model.type, dots$covariate, dots$first.imported.year, dots$last.imported.year,
        dots$duration, dots$last.analysed.year, dots$analysis.date, dots$seed, parent
      ),
      algo = "sha1"
    )
    check_dataframe_variable(
      df = dots$parent.status,
      name = "parent.status",
      variable = c("FileFingerprint", "StatusFingerprint", "Status")
    )
    dots$parent.status <- dots$parent.status[
      order(dots$parent.status$FileFingerprint),
      c("FileFingerprint", "StatusFingerprint", "Status")
    ]
    parameter <- data.frame(
      Parent = character(0),
      Value = numeric(0),
      Estimate = numeric(0),
      Variance = numeric(0),
      stringsAsFactors = FALSE
    )
    index <- data.frame(
      Value = numeric(0),
      Estimate = numeric(0),
      LowerConfidenceLimit = numeric(0),
      UpperConfidenceLimit = numeric(0),
      stringsAsFactors = FALSE
    )
    
    status.fingerprint <- digest(
      list(
        file.fingerprint, dots$status, dots$parent.status, parameter, index, sessionInfo()
      ),
      algo = "sha1"
    )
    
    new(
      "n2kComposite",
      Status = dots$status,
      SchemeID = dots$scheme.id,
      SpeciesGroupID = dots$species.group.id,
      LocationGroupID = dots$location.group.id,
      ModelType = dots$model.type,
      Covariate = dots$covariate,
      FirstImportedYear = dots$first.imported.year,
      LastImportedYear = dots$last.imported.year,
      Duration = dots$duration,
      LastAnalysedYear = dots$last.analysed.year,
      AnalysisDate = dots$analysis.date,
      Seed = dots$seed,
      FileFingerprint = file.fingerprint,
      StatusFingerprint = status.fingerprint,
      SessionInfo = sessionInfo(),
      Parent = parent,
      ParentStatus = dots$parent.status,
      Parameter = parameter,
      Index = index
    )
  }
)
