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
#' @param which.parent 0 = the parent for the null hypothesis, 1 =  the parent for the alternative hypothesis
setMethod(
  f = "get_parent",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, which.parent = c(0, 1)){
    which.parent <- match.arg(which.parent)
    if (which.parent == 0) {
      return(x@Parent0)
    } else {
      return(x@Parent)
    }
  }
)

#' @rdname get_parent
#' @aliases get_parent,n2kComposite-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_parent",
  signature = signature(x = "n2kComposite"),
  definition = function(x, ...){
    return(x@Parent)
  }
)
