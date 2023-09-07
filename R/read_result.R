#' Read an `n2kResult` object
#' @param x the file fingerprint of the `n2kResult`
#' @param base the base location to read the results
#' @param project will be a relative path within the base location
#' @name read_result
#' @rdname read_result
#' @exportMethod read_result
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "read_result",
  def = function(x, base, project) {
    standardGeneric("read_result") # nocov
  }
)

#' @rdname read_result
#' @importFrom assertthat assert_that is.string is.dir noNA
#' @importFrom methods setMethod new
#' @importFrom fs file_exists path
setMethod(
  f = "read_result",
  signature = signature(base = "character"),
  definition = function(x, base, project) {
    assert_that(
      is.string(x), noNA(x), grepl("^[[:xdigit:]]{40}", x), is.dir(base),
      is.string(project), noNA(project), is.dir(path(base, project, "results"))
    )
    filename <- path(base, project, "results", x, ext = "rds")
    assert_that(
      file_exists(filename), msg = sprintf("`%s` does not exists", filename)
    )
    readRDS(filename)
  }
)

#' @rdname read_result
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom aws.s3 bucket_exists get_bucket s3readRDS
#' @include import_s3_classes.R
setMethod(
  f = "read_result",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project) {
    assert_that(
      is.string(x), noNA(x), grepl("^[[:xdigit:]]{40}$", x), is.string(project),
      noNA(project)
    )
    prefix <- file.path(project, "results", paste0(x, ".rds"), fsep = "/")
    available <- get_bucket(base, prefix = prefix, max = Inf)
    if (length(available) == 1) {
      return(s3readRDS(available))
    }
    stopifnot("no matching object in bucket" = length(available) > 0)
    stop("multiple objects matching `", x, "` in bucket")
  }
)

#' @rdname read_result
#' @importFrom methods setMethod
setMethod(
  f = "read_result",
  signature = signature(base = "ANY"),
  definition = function(x, base, project) {
    stop("base must be either a directory or an S3 bucket")
  }
)
