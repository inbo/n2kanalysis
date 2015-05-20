#' Create a n2kLrtGlmer object
#' @param parent the file fingerprint of the paretnt
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
#' @name n2k_lrt_glmer
#' @rdname n2k_lrt_glmer
#' @exportMethod n2k_lrt_glmer
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_lrt_glmer", 
  def = function(
    parent, ..., model.fit
  ){
    standard.generic("n2k_lrt_glmer")
  }
)

#' @description A new n2kLrtGlmer model is created when \code{parent} is a character
#' @rdname n2k_lrt_glmer
#' @aliases n2k_lrt_glmer,n2kLrtGlmer-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "n2k_lrt_glmer", 
  signature = signature(parent = "character"),
  definition = function(
    parent, ..., model.fit
  ){
    dots <- list(...)
    parent <- check_single_character(parent, name = "parent")
    dots$parent.0 <- check_single_character(dots$parent.0, name = "parent.0")
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
        dots$duration, dots$last.analysed.year, dots$analysis.date, dots$seed, parent, 
        dots$parent.0
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
    status.fingerprint <- digest(
      list(
        file.fingerprint, dots$status, dots$parent.status, NULL, NULL, NULL, sessionInfo()
      ),
      algo = "sha1"
    )
    
    new(
      "n2kLrtGlmer",
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
      Parent0 = dots$parent.0,
      ParentStatus = dots$parent.status,
      Model = NULL,
      Model0 = NULL,
      Anova = NULL
    )
  }
)

#' @description In case \code{parent} a n2kLrtGlmer object is, then only the model and status are updated. All other slots are unaffected.
#' @rdname n2k_lrt_glmer
#' @aliases n2k_lrt_glmer,my_lmer-methods
#' @importFrom methods setMethod validObject
#' @importFrom digest digest
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "n2k_lrt_glmer", 
  signature = signature(parent = "n2kLrtGlmer", model.fit = "glmerMod"),
  definition = function(
    parent, ..., model.fit
  ){
    dots <- list(...)
    if(is.null(dots$which.model)){
      warning("'which.model' unspecified, assuming which.model = 1")
      dots$which.model <- 1L
    }
    if(!class(dots$which.model) %in% c("integer", "numeric")){
      stop("'which.model' must be integer")
    }
    if(length(dots$which.model) != 1){
      stop("'which.model' must be a single number")
    }
    if(is.numeric(dots$which.model)){
      if(abs(dots$which.model - round(dots$which.model)) > 1e-8){
        stop("'which.model' is not integer")
      } else {
        dots$which.model <- as.integer(dots$which.model)
      }
    }
    if(!dots$which.model %in% 0:1){
      stop("'which.model' must be 0 (NULL model) or 1 (alternative model)")
    }
    if(dots$which.model == 0){
      parent@Model0 <- model.fit
    } else {
      parent@Model <- model.fit
    }
    if(is.null(dots$status)){
      if(dots$which.model == 0 & !is.null(parent@Model)){
        parent@Status <- "new"
      }
      if(dots$which.model == 1 & !is.null(parent@Model0)){
        parent@Status <- "new"
      }
    }
    parent@SessionInfo <- sessionInfo()
    parent@StatusFingerprint <- digest(
      list(
        parent@FileFingerprint, parent@Status, parent@ParentStatus, parent@Model, 
        parent@Model0, parent@Anova, parent@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(parent)
    return(parent)
  }
)
