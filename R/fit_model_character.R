#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom aws.s3 get_bucket
#' @param base the root of a project. Can be either a directory on a file system or an AWS S3 bucket object. Extracted from `bucket` or `x` when missing.
#' @param project the subdirectory of the project. Is relative the `base`. Extracted from `x` when missing.
#' @param status A vector with status levels naming the levels which should be calculated. Defaults to `"new"`.
#' @param verbose A logical indicating if the function should display the name of the file and the status. Defaults to `TRUE`.
#' @param bucket the name of the AWS S3 bucket. Only used when `base` is missing.
setMethod(
  f = "fit_model",
  signature = signature(x = "character"),
  definition = function(
    x, base, project, status = "new", verbose = TRUE, ..., bucket
  ){
    assert_that(is.string(x))
    assert_that(is.flag(verbose))
    if (isTRUE(verbose)) {
      message(x)
    }
    manifest <- grepl("\\.manifest$", x)
    if (manifest) {
      pattern <- "(.*\\/)?(.*)\\/+manifest\\/([[:xdigit:]]{40})\\.manifest"
    } else {
      pattern <-
        "(.*\\/)?(.*)\\/+[[:xdigit:]]{4}\\/.*\\/([[:xdigit:]]{40})\\.rds$"
    }
    if (missing(project)) {
      project <- gsub(pattern = pattern, replacement = "\\2", x = x)
    }
    if (missing(base)) {
      if (missing(bucket)) {
        base <- gsub(pattern = pattern, replacement = "\\1", x = x) %>%
          gsub(pattern = "\\/$", replacement = "")
      } else {
        base <- get_bucket(bucket, prefix = project, max = 1)
      }
    }
    hash <- gsub(pattern, "\\3", x)
    if (manifest) {
      read_manifest(hash, base = base, project = project) %>%
        fit_model(base = base, project = project, verbose = verbose, ...)
      return(invisible(NULL))
    }
    analysis <- read_model(hash, base = base, project = project)
    if (isTRUE(verbose)) {
      message(status(analysis), " -> ", appendLF = FALSE)
      utils::flush.console()
    }
    analysis <- fit_model(
      x = analysis,
      status = status,
      base = base,
      project = project
    )
    if (verbose) {
      message(status(analysis))
      utils::flush.console()
    }
    store_model(analysis, base = base, project = project)
    rm(analysis)
    gc(verbose = FALSE)
    return(invisible(NULL))
  }
)
