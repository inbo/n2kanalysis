#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom aws.s3 s3readRDS
#' @details
#' - `status`: A vector with status levels naming the levels which should be
#' recalculated.
#' Defaults to `"new"`.
#' - `verbose`: A logical indicating if the function should display the name of
#' the file and the status.
#' Defaults to `TRUE`.
#' @include import_S3_classes.R
setMethod(
  f = "fit_model",
  signature = signature(x = "s3_object"),
  definition = function(x, ...) {
    dots <- list(...)
    if (is.null(dots$verbose)) {
      dots$verbose <- TRUE
    }
    display(dots$verbose, x$Key)
    if (is.null(dots$base)) {
      dots$base <- get_bucket(x$Bucket)
    }
    if (is.null(dots$project)) {
      dots$project <- gsub(
        pattern = "(.*)/(.*)/([[:xdigit:]]{1,40})\\.(rds|manifest)$",
        replacement = "\\1",
        x$Key
      )
    }
    if (grepl("\\.manifest$", x$Key)) {
      hash <- gsub(".*?([[:xdigit:]]{1,40}).manifest$", "\\1", x$Key)
      read_manifest(base = dots$base, project = dots$project, hash = hash) %>%
        fit_model(base = dots$base, project = dots$project, ...)
      return(invisible(NULL))
    }
    analysis <- s3readRDS(object = x)
    current_status <- status(analysis)
    display(dots$verbose, paste(status(analysis), "-> "), FALSE)
    analysis.fitted <- fit_model(
      x = analysis,
      status = dots$status,
      base = dots$base,
      project = dots$project
    )
    display(dots$verbose, status(analysis.fitted))
    store_model(
      analysis.fitted,
      base = dots$base,
      project = dots$project
    )
    return(invisible(NULL))
  }
)
