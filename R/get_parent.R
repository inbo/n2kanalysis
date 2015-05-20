#' Get the fingerprint of the parent analysis of a n2kModel object
#' @param x the n2kModel object
#' @param ... additional arguments (see details)
#' @return the parent of the object
#' @name get_parent
#' @rdname get_parent
#' @exportMethod get_parent
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_parent", 
  def = function(x, ...){
    standard.generic("get_parent")
  }
)

#' @rdname get_parent
#' @aliases get_parent,n2kLrtGlmer-methods
#' @importFrom methods setMethod
#' @details
#' \describe{
#'  \item{\code{which.parent}}{0 = the parent for the null hypothesis, 1 =  the parent for the alternative hypothesis}
#' }
setMethod(
  f = "get_parent",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, ...){
    dots <- list(...)
    if(is.null(dots$which.parent)){
      warning("'which.parent' unspecified, assuming which.parent = 1")
      dots$which.parent <- 1L
    }
    if(!class(dots$which.parent) %in% c("integer", "numeric")){
      stop("'which.parent' must be integer")
    }
    if(length(dots$which.parent) != 1){
      stop("'which.parent' must be a single number")
    }
    if(is.numeric(dots$which.parent)){
      if(abs(dots$which.parent - round(dots$which.parent)) > 1e-8){
        stop("'which.parent' is not integer")
      } else {
        dots$which.parent <- as.integer(dots$which.parent)
      }
    }
    if(!dots$which.parent %in% 0:1){
      stop("'which.parent' must be 0 (NULL model) or 1 (alternative model)")
    }
    if(dots$which.parent == 0){
      return(x@Parent0)
    } else {
      return(x@Parent)
    }
  }
)
