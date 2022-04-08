#' Read an `n2kModel` object
#' @param x the file fingerprint of the `n2kModel`
#' @param base the base location to read the model
#' @param project will be a relative path within the base location
#' @name read_model
#' @rdname read_model
#' @exportMethod read_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "read_model",
  def = function(x, base, project) {
    standardGeneric("read_model") # nocov
  }
)

#' @rdname read_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.dir
#' @importFrom utils file_test
setMethod(
  f = "read_model",
  signature = signature(base = "character"),
  definition = function(x, base, project) {
    assert_that(is.string(x))
    assert_that(is.dir(base))
    assert_that(is.string(project))

    filename <- file.path(base, project, substring(x, 1, 4), fsep = "/") %>%
      normalizePath() %>%
      list.files(pattern = x, full.names = TRUE, recursive = TRUE)
    filename <- filename[grepl("\\.rds$", filename)]

    if (length(filename) == 1) {
      return(readRDS(filename))
    }

    if (length(filename) == 0) {
      stop("no matching object in directory")
    }
    stop("multiple matching objects in directory")
  }
)

#' @rdname read_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom purrr map_chr
#' @importFrom aws.s3 bucket_exists get_bucket s3readRDS
#' @include import_s3_classes.R
setMethod(
  f = "read_model",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project) {
    assert_that(is.string(x))
    assert_that(is.string(project))

    prefix <- file.path(project, substring(x, 1, 4), "", fsep = "/")
    available <- get_bucket(base, prefix = prefix, max = Inf)
    map_chr(available, "Key") %>%
      basename() %>%
      grep(pattern = x) -> matching
    if (length(matching) == 1) {
      return(s3readRDS(available[[matching]]))
    }

    if (length(matching) == 0) {
      stop("no matching object in bucket")
    }
    stop("multiple matching objects in bucket")
  }
)
