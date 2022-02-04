#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom multimput model_impute
#' @include n2k_model_imputed_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kModelImputed"),
  definition = function(x, ...) {
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # status: "waiting"
    if (status(x) == "waiting" | is.null(x@AggregatedImputed)) {
      parent <- get_parents(x, base = dots$base, project = dots$project)
      if (length(parent) == 0) {
        stop("parent analysis not found")
      }
      if (length(parent) > 1) {
        stop("Multiple parents")
      }
      parent_status <- status(parent[[1]])
      if (parent_status %in% c("new", "waiting")) {
        status(x) <- "waiting"
        return(x)
      }
      if (parent_status != "converged") {
        status(x) <- "error"
        return(x)
      }
      x@AggregatedImputed <- parent[[1]]@AggregatedImputed
      x@AnalysisRelation$parent_status <- parent[[1]]@AnalysisMetadata$status
      x@AnalysisRelation$parentstatus_fingerprint <-
        parent[[1]]@AnalysisMetadata$status_fingerprint
      status(x) <- "new"
    }
    sapply(x@Package, require, quietly = TRUE, character.only = TRUE)
    if (length(x@PrepareModelArgs)) {
      model_args <- c(x@ModelArgs, x@PrepareModelArgs[[1]](x))
    } else {
      model_args <- x@ModelArgs
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