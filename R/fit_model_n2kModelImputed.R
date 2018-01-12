#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom multimput model_impute
#' @importFrom aws.s3 get_bucket s3readRDS
#' @include n2kModelImputed_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kModelImputed"),
  definition = function(x, ...){
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
        stop("Parent analysis not found")
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
      x@AnalysisRelation$ParentStatus <- parent[[1]]@AnalysisMetadata$Status
      x@AnalysisRelation$ParentStatusFingerprint <-
        parent[[1]]@AnalysisMetadata$StatusFingerprint
      status(x) <- "new"
    }
    sapply(x@Package, require, quietly = TRUE, character.only = TRUE)
    model <- try(
      model_impute(
        object = x@AggregatedImputed,
        model.fun = x@Function,
        rhs = gsub("~", "", x@AnalysisMetadata$Formula),
        model.args = x@ModelArgs,
        extractor = x@Extractor,
        extractor.args = x@ExtractorArgs,
        filter = x@Filter,
        mutate = x@Mutate
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
