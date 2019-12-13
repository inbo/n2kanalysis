#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kInlaComparison_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInlaComparison"),
  definition = function(analysis, ...) {
    warning("reading model parameters on n2kInlaComparison is to do")
    return(new("n2kParameter"))
  }
)
