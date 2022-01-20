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
  def = function(x) {
    standardGeneric("get_model") # nocov
  }
)

#' @rdname get_model
#' @aliases get_model,n2kInla-methods
#' @importFrom methods setMethod new
#' @include n2k_inla_class.R
setMethod(
  f = "get_model",
  signature = signature(x = "n2kInla"),
  definition = function(x) {
    return(x@Model)
  }
)

#' @rdname get_model
#' @aliases get_model,character-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that
#' @importFrom utils file_test
#' @importFrom n2khelper read_object_environment
setMethod(
  f = "get_model",
  signature = signature(x = "character"),
  definition = function(x) {
    assert_that(file_test("-f", x))
    return(get_model(readRDS(x)))
  }
)
