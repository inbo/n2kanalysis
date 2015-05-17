#' Get the session info of a n2kModel object
#' @param x the n2kModel object
#' @return the seed of the object
#' @name get_session_info
#' @rdname get_session_info
#' @exportMethod get_session_info
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_session_info", 
  def = function(x){
    standard.generic("get_session_info")
  }
)

#' @rdname get_session_info
#' @aliases get_session_info,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_session_info",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@SessionInfo)
  }
)
