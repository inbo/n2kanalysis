#' Create a n2kGlmerPoisson object
#' @param data a data.frame with the data to analyse
#' @param model.fit The fitted model
#' @param ... other arguments. See below
#' @details
#'   \describe{
#'    \item{\code{status}}{a single character indicating the status of the model. Defaults to 'new' when \code{data} is a data.frame.}
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
#'    \item{\code{weight}}{The name of the variable to use as weights. '' indicates no weighting. Defaults to ''}
#'    \item{\code{seed}}{a single integer used as a seed for all calculations. A random seed will be inserted when missing.}
#'   }
#' @name n2k_glmer_poisson
#' @rdname n2k_glmer_poisson
#' @exportMethod n2k_glmer_poisson
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_glmer_poisson", 
  def = function(
    data, ..., model.fit
  ){
    standard.generic("n2k_glmer_poisson")
  }
)

#' @description A new n2kGlmerPoisson model is created when \code{data} is a data.frame.
#' @rdname n2k_glmer_poisson
#' @aliases n2k_glmer_poisson,n2kGlmerPoisson-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "n2k_glmer_poisson", 
  signature = signature(data = "data.frame"),
  definition = function(
    data, ..., model.fit
  ){
    dots <- list(...)
    #set the defaults for missing arguments in dots
    if(is.null(dots$status)){
      dots$status <- "new"
    }
    if(is.null(dots$weight)){
      dots$weight <- ""
    }
    if(is.null(dots$seed)){
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
        data, dots$scheme.id, dots$species.group.id, dots$location.group.id, 
        dots$model.type, dots$covariate, dots$first.imported.year, dots$last.imported.year,
        dots$duration, dots$last.analysed.year, dots$analysis.date, dots$seed, dots$weight
      ),
      algo = "sha1"
    )

    new(
      "n2kGlmerPoisson",
      Data = data,
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
      Weight = dots$weight,
      DataFingerprint = digest(data, algo = "sha1"),
      FileFingerprint = file.fingerprint,
      Model = NULL
    )
  }
)

#' @description In case \code{data} a n2kGlmerPoisson object is, then only the model and status are updated. All other slots are unaffected.
#' @rdname n2k_glmer_poisson
#' @aliases n2k_glmer_poisson,my_lmer-methods
#' @importFrom methods setMethod validObject
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "n2k_glmer_poisson", 
  signature = signature(data = "n2kGlmerPoisson", model.fit = "glmerMod"),
  definition = function(
    data, ..., model.fit
  ){
    dots <- list(...)
    data@Model <- model.fit
    data@Status <- dots$status
    validObject(data)
    return(data)
  }
)
