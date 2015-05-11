#' Create a n2kInlaNbinomial object
#' @inheritParams n2k_glmer_poisson
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
    if(is.null(dots$status)){
      dots$status <- "new"
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
    dots$analysis.date <- check_single_posix(
      dots$analysis.date, 
      name = "analysis.date", 
      past = TRUE
    )
    file.fingerprint <- digest(
      list(
        data, dots$scheme.id, dots$species.group.id, dots$location.group.id, 
        dots$model.type, dots$covariate, dots$first.imported.year, dots$last.imported.year,
        dots$analysis.date, dots$seed
      ),
      algo = "sha1"
    )

    new(
      "n2kInlaNbinomial",
      Data = data,
      Status = dots$status,
      SchemeID = dots$scheme.id,
      SpeciesGroupID = dots$species.group.id,
      LocationGroupID = dots$location.group.id,
      ModelType = dots$model.type,
      Covariate = dots$covariate,
      FirstImportedYear = dots$first.imported.year,
      LastImportedYear = dots$last.imported.year,
      AnalysisDate = dots$analysis.date,
      Seed = dots$seed,
      DataFingerprint = digest(data, algo = "sha1"),
      FileFingerprint = file.fingerprint,
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
    data@Status <- dots$status
    validObject(data)
    return(data)
  }
)
