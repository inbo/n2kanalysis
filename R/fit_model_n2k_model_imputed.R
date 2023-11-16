#' @rdname fit_model
#' @importFrom dplyr coalesce
#' @importFrom methods setMethod new
#' @importFrom multimput model_impute
#' @include n2k_model_imputed_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kModelImputed"),
  definition = function(x, ...) {
    validObject(x)
    dots <- list(...)
    dots$status <- coalesce(list(dots$status), list(c("new", "waiting")))[[1]]

    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # status: "waiting"
    if (status(x) != "new" || is.null(x@AggregatedImputed)) {
      parent <- get_parents(x, base = dots$base, project = dots$project)
      if (length(parent) != 1) {
        status(x) <- "error"
        return(x)
      }
      parent_status <- status(parent[[1]])
      if (parent_status != "converged") {
        status(x) <- ifelse(
          parent_status %in% c("new", "waiting"), "waiting", "error"
        )
        x@AnalysisRelation$parent_status <- parent[[1]]@AnalysisMetadata$status
        x@AnalysisRelation$parentstatus_fingerprint <-
          parent[[1]]@AnalysisMetadata$status_fingerprint
        return(x)
      }
      x@AggregatedImputed <- parent[[1]]@AggregatedImputed
      x@AnalysisRelation$parent_status <- parent[[1]]@AnalysisMetadata$status
      x@AnalysisRelation$parentstatus_fingerprint <-
        parent[[1]]@AnalysisMetadata$status_fingerprint
      status(x) <- "new"
    }
    stopifnot(all(vapply(
      x@Package, FUN = require, FUN.VALUE = logical(1), quietly = TRUE,
      character.only = TRUE
    )))
    model_args <- x@ModelArgs
    if (length(x@PrepareModelArgs)) {
      model_args <- c(model_args, x@PrepareModelArgs[[1]](x))
    }
    model <- try(
      model_impute(
        object = x@AggregatedImputed, model_fun = x@Function,
        rhs = gsub("~", "", x@AnalysisMetadata$formula),
        model_args = model_args, extractor = x@Extractor,
        extractor_args = x@ExtractorArgs, filter = x@Filter, mutate = x@Mutate
      )
    )
    if ("try-error" %in% class(model)) {
      status(x) <- "error"
      return(x)
    }
    x@Results <- model
    status(x) <- "converged"
    return(x)
  }
)
