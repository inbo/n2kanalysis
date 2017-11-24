#' Store a n2kModel object
#' @param x the n2kModel
#' @param base the base location to store the model
#' @param project will be a relative path within the base location
#' @param overwrite should an existing object be overwritten? Defaults to TRUE
#' @name store_model
#' @rdname store_model
#' @exportMethod store_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_model",
  def = function(x, base, project, overwrite = TRUE){
    standardGeneric("store_model") # nocov
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag noNA
setMethod(
  f = "store_model",
  signature = signature(base = "character"),
  definition = function(x, base, project, overwrite = TRUE){
    assert_that(is.flag(overwrite))
    assert_that(noNA(overwrite))
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(base))
    assert_that(file_test("-d", base))
    assert_that(is.string(project))
    validObject(x, complete = TRUE)

    status <- status(x)
    fingerprint <- get_file_fingerprint(x)

    #create dir is it doesn't exist
    dir <- sprintf("%s/%s/%s", base, project, status) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    current <- sprintf("%s/%s", base, project) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      list.files(
        pattern = sprintf("%s.rds$", fingerprint),
        full.names = TRUE,
        recursive = TRUE
      )
    filename <- sprintf("%s/%s.rds", dir, fingerprint) %>%
      normalizePath(winslash = "/", mustWork = FALSE)

    if (length(current) > 0) {
      if (!overwrite) {
        return(current)
      }
      file.remove(current)
    }

    saveRDS(x, file = filename)
    return(filename)
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom aws.s3 bucket_exists get_bucket s3saveRDS delete_object
#' @include import_S3_classes.R
setMethod(
  f = "store_model",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project, overwrite = TRUE){
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(project))
    validObject(x, complete = TRUE)

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

    status <- status(x)
    fingerprint <- get_file_fingerprint(x)

    existing <- get_bucket(base, prefix = project, max = Inf)
    existing <- existing[names(existing) == "Contents"] %>%
      sapply("[[", "Key")
    current <- existing[grepl(sprintf("%s.rds$", fingerprint), existing)]
    filename <- sprintf("%s/%s/%s.rds", project, status, fingerprint) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      gsub(pattern = "//", replacement = "/") %>%
      gsub(pattern = "^/", replacement = "")

    if (length(current) > 0) {
      if (!overwrite) {
        return(current)
      }
      delete_object(object = current, bucket = base)
    }

    # try several times to write to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bucket_ok <- tryCatch(
        s3saveRDS(x, bucket = base, object = filename),
        error = function(err) {
          err
        }
      )
      if (is.logical(bucket_ok)) {
        break
      }
      if (i > 10) {
        stop("Unable to write to S3 bucket")
      }
      message("attempt ", i, " to write to S3 bucket failed. Trying again...")
      i <- i + 1
      # waiting time between tries increases with the number of tries
      Sys.sleep(i)
    }
    if (!bucket_ok) {
      stop("Unable to write to S3 bucket")
    }

    return(filename)
  }
)
