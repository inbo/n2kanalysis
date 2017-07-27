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
    if (status(x) == "waiting") {
      if (is.null(dots$path)) {
        dots$path <- "."
      }
      if (inherits(dots$path, "s3_object")) {
        available <- get_bucket(
          bucket = dots$path$Bucket,
          prefix = gsub(
            "(.*)/(.*)/(.*)\\.rds",
            sprintf("\\1/converged/%s.rds", x@AnalysisRelation$ParentAnalysis),
            dots$path$Key
          )
        )
        if (length(available) == 0) {
          available <- get_bucket(
            bucket = dots$path$Bucket,
            prefix = gsub(
              "(.*)/(.*)/(.*)\\.rds",
              sprintf("\\1/new/%s.rds", x@AnalysisRelation$ParentAnalysis),
              dots$path$Key
            )
          )
          if (length(available) == 1) {
            return(x)
          }
          available <- get_bucket(
            bucket = dots$path$Bucket,
            prefix = gsub(
              "(.*)/(.*)/(.*)\\.rds",
              sprintf("\\1/waiting/%s.rds", x@AnalysisRelation$ParentAnalysis),
              dots$path$Key
            )
          )
          if (length(available) == 1) {
            return(x)
          } else {
            stop(
"Parent analysis has status different from converged, new of waiting. To do..."
            )
          }
        }
        parent <- s3readRDS(object = available[[1]])
      } else {
        parent <- list.files(
          dots$path,
          pattern = x@AnalysisRelation$ParentAnalysis,
          recursive = TRUE,
          full.names = TRUE
        )
        if (length(parent) == 0) {
          stop("Parent analysis not found")
        }
        if (length(parent) > 1) {
          stop("Multiple parents")
        }
        if (grepl("/(new|waiting)/[0-9a-f]{40}.rds$", parent)) {
          status(x) <- "waiting"
          return(x)
        }
        if (grepl("/error/[0-9a-f]{40}.rds$", parent)) {
          status(x) <- "error"
          return(x)
        }
        if (grepl("/converged/[0-9a-f]{40}.rds$", parent)) {
          parent <- readRDS(parent)
        } else {
            stop(
"Parent analysis has status different from converged, new, waiting or error. To do..."
            )
        }
      }
      x@AggregatedImputed <- parent@AggregatedImputed
      x@AnalysisRelation$ParentStatus <- parent@AnalysisMetadata$Status
      x@AnalysisRelation$ParentStatusFingerprint <-
        parent@AnalysisMetadata$StatusFingerprint
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
