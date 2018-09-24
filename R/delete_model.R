#' Delete a n2kModel object
#' @param x the file fingerprint of the n2kModel
#' @param base the base location
#' @param project will be a relative path within the base location
#' @name delete_model
#' @rdname delete_model
#' @exportMethod delete_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "delete_model",
  def = function(x, base, project){
    standardGeneric("delete_model") # nocov
  }
)

#' @rdname delete_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.dir
#' @importFrom utils file_test
setMethod(
  f = "delete_model",
  signature = signature(base = "character"),
  definition = function(x, base, project){
    assert_that(is.string(x))
    assert_that(is.dir(base))
    assert_that(is.string(project))

    filename <- file.path(base, project) %>%
      normalizePath() %>%
      list.files(pattern = x, full.names = TRUE, recursive = TRUE)
    filename <- filename[grepl("\\.rds$", filename)]

    if (length(filename) == 0) {
      warning("no matching object in directory")
      return(invisible(NULL))
    }

    unlink(filename)
  }
)

#' @rdname delete_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom aws.s3 bucket_exists get_bucket delete_object
#' @importFrom purrr map
#' @include import_S3_classes.R
setMethod(
  f = "delete_model",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project){
    assert_that(is.string(x))
    assert_that(is.string(project))

    # try several times to connect to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bucket_ok <- tryCatch(
        bucket_exists(base),
        error = function(err) {
          err
        }
      )
      if (is.logical(bucket_ok)) {
        break
      }
      if (i > 10) {
        stop("Unable to connect to S3 bucket")
      }
      message("attempt ", i, " to connect to S3 bucket failed. Trying again...")
      i <- i + 1
      # waiting time between tries increases with the number of tries
      Sys.sleep(i)
    }
    if (!bucket_ok) {
      stop("Unable to connect to S3 bucket")
    }

    # check if object with same fingerprint exists
    available <- get_bucket(base, prefix = project, max = Inf)
    existing <- available[names(available) == "Contents"] %>%
      sapply("[[", "Key")
    matching <- sprintf("%s.rds", x) %>%
      grep(existing)
    if (length(matching) == 0) {
      warning("no matching object in bucket")
      return(invisible(NULL))
    }
    map(available[matching], delete_object)
  }
)
