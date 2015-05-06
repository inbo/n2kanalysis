#' Create a n2kGlmerPoisson object
#' @param data a data.frame with the data to analyse
#' @param model.fit The fitted model
#' @param ... other arguments
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

#' @rdname n2k_glmer_poisson
#' @aliases n2k_glmer_poisson,n2kGlmerPoisson-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_single_strictly_positive_integer
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
    
    new(
      "n2kGlmerPoisson",
      Data = data,
      Status = dots$status,
      Seed = dots$seed,
      Weight = dots$weight,
      Model = NULL
    )
  }
)

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
