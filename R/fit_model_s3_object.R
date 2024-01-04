#' @rdname fit_model
#' @importFrom aws.s3 s3readRDS
#' @importFrom dplyr coalesce
#' @importFrom methods setMethod new
#' @details
#' - `status`: A vector with status levels naming the levels which should be
#' recalculated.
#' Defaults to `c("new", "waiting")`.
#' - `verbose`: A logical indicating if the function should display the name of
#' the file and the status.
#' Defaults to `TRUE`.
#' @include import_s3_classes.R
setMethod(
  f = "fit_model",
  signature = signature(x = "s3_object"),
  definition = function(x, status = c("new", "waiting"), ...) {
    assert_that(is.character(status), length(status) >= 1)
    dots <- list(...)
    dots$verbose <- coalesce(dots$verbose, TRUE)
    display(dots$verbose, x$Key)
    dots$base <- get_bucket(x$Bucket)
    dots$project <- gsub(
      pattern = "(.*)/(.*)/([[:xdigit:]]{1,40})\\.(rds|manifest)$",
      replacement = "\\1",
      x$Key
    )
    if (grepl("\\.manifest$", x$Key)) {
      hash <- gsub(".*?([[:xdigit:]]{1,40}).manifest$", "\\1", x$Key)
      read_manifest(base = dots$base, project = dots$project, hash = hash) |>
        fit_model(base = dots$base, project = dots$project, ...) -> output
      return(invisible(output))
    }
    if (all(!vapply(status, FUN = grepl, FUN.VALUE = logical(1), x$Key))) {
      display(dots$verbose, "skipped")
      return(invisible(NULL))
    }
    analysis <- s3readRDS(object = x)
    display(dots$verbose, paste(status(analysis), "-> "), FALSE)
    analysis_fitted <- fit_model(
      x = analysis, status = status, base = dots$base,
      project = dots$project, ...
    )
    display(dots$verbose, status(analysis_fitted))
    store_model(analysis_fitted, base = dots$base, project = dots$project)
    return(invisible(NULL))
  }
)
