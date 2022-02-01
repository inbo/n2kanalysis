#' Extract the relevant coefficients
#' @param extractor the extractor function
#' @param object the n2kModel object
#' @param base the optional base location of the object
#' @param project the optional subdirectory
#' @return the relevant coefficients
#' @name extract
#' @rdname extract
#' @exportMethod extract
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "extract",
  def = function(extractor, object, base, project) {
    # nocov start
    standardGeneric("extract")
    # nocov end
  }
)

#' @rdname extract
#' @aliases extract,character-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper read_object_environment
#' @importFrom dplyr %>% bind_rows
setMethod(
  f = "extract",
  signature = signature(object = "character"),
  definition = function(extractor, object, base, project) {
    if (length(object) > 1) {
      output <- lapply(
        object,
        extract,
        base = base,
        project = project,
        extractor = extractor
      ) %>%
        bind_rows()
      return(output)
    }
    parent <- read_model(object, base = base, project = project)
    cbind(
      parent = object,
      extract(
        extractor = extractor,
        object = parent,
        base = base,
        project = project
      )
    )
  }
)

#' @rdname extract
#' @aliases extract,n2kInla-methods
#' @importFrom methods setMethod new
#' @include n2k_inla_class.R
setMethod(
  f = "extract",
  signature = signature(object = "n2kInla"),
  definition = function(extractor, object, base = NULL, project = NULL) {
    assert_that(inherits(extractor, "function"))
    extractor(object@Model)
  }
)
