#' Extract the relevant coefficients
#' @param extractor the extractor function
#' @param object the n2kModel object
#' @param path the optional path
#' @return the relevant coefficients
#' @name extract
#' @rdname extract
#' @exportMethod extract
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "extract",
  def = function(extractor, object, path){
    # nocov start
    standard.generic("extract")
    # nocov end
  }
)

#' @rdname extract
#' @aliases extract,character-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_path read_object_environment
setMethod(
  f = "extract",
  signature = signature(object = "character"),
  definition = function(extractor, object, path){
    if (length(object) > 1) {
      output <- do.call(
        rbind,
        lapply(
          object,
          extract,
          path = path,
          extractor = extractor
        )
      )
      return(output)
    }
    path <- check_path(path, type = "directory")
    file <- paste0(path, "/", object, ".rda")
    file <- check_path(file, type = "file")
    local.environment <- new.env()
    load(file, envir = local.environment)
    parent <- read_object_environment(
      object = "analysis",
      env = local.environment
    )
    cbind(
      Parent = object,
      extract(extractor = extractor, object = parent, path = NULL)
    )
  }
)

#' @rdname extract
#' @aliases extract,n2kInlaNbinomial-methods
#' @importFrom methods setMethod
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "extract",
  signature = signature(object = "n2kInlaNbinomial"),
  definition = function(extractor, object, path = NULL){
    assert_that(inherits(extractor, "function"))
    extractor(object@Model)
  }
)
