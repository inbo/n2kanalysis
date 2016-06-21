#' Get the model of a n2kModel object
#' @param x the n2kModel object
#' @return the model of the object
#' @name get_model
#' @rdname get_model
#' @exportMethod get_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model",
  def = function(x){
    standardGeneric("get_model") # nocov
  }
)

#' @rdname get_model
#' @aliases get_model,n2kGlmerPoisson-methods
#' @importFrom methods setMethod new
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "get_model",
  signature = signature(x = "n2kGlmerPoisson"),
  definition = function(x){
    return(x@Model)
  }
)

#' @rdname get_model
#' @aliases get_model,n2kInlaNbinomial-methods
#' @importFrom methods setMethod new
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "get_model",
  signature = signature(x = "n2kInlaNbinomial"),
  definition = function(x){
    return(x@Model)
  }
)

#' @rdname get_model
#' @aliases get_model,character-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_path read_object_environment
setMethod(
  f = "get_model",
  signature = signature(x = "character"),
  definition = function(x){
    x <- check_path(x, type = "file")
    local.environment <- new.env()
    load(x, envir = local.environment)
    analysis <- read_object_environment(
      object = "analysis",
      env = local.environment
    )
    return(get_model(analysis))
  }
)
