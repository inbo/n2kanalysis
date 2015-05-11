#' Get the model set of a n2kModel object
#' @param x the n2kModel object
#' @return the model set of the object
#' @name get_model_set
#' @rdname get_model_set
#' @exportMethod get_model_set
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_model_set", 
  def = function(x){
    standard.generic("get_model_set")
  }
)

#' @rdname get_model_set
#' @aliases get_model_set,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_model_set",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(data.frame(
      ModelType = x@ModelType,
      FirstImportedYear = x@FirstImportedYear,
      LastImportedYear = x@LastImportedYear,
      Duration = x@Duration
    ))
  }
)
