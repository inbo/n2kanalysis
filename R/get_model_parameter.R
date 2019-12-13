#' Add the model parameters from a model
#' @param analysis The model to add.
#' @param ... extra options
#' @name get_model_parameter
#' @rdname get_model_parameter
#' @exportMethod get_model_parameter
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model_parameter",
  def = function(analysis, ...) {
    standardGeneric("get_model_parameter") # nocov
  }
)
