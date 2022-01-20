#' @rdname get_result
#' @importFrom methods setMethod new
#' @include import_s3_classes.R
setMethod(
  f = "get_result",
  signature = signature(x = "s3_object"),
  definition = function(x, ...) {
    x <- s3readRDS(object = x)
    get_result(x, ...)
  }
)
