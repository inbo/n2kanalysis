#' Store a n2kModel object
#' @param x The n2kModel.
#' @param base The base location to store the model.
#' @param project Will be a relative path within the base location.
#' @param overwrite Should an existing object be overwritten?
#' Defaults to TRUE.
#' @param validate Check that the object is valid before storing it.
#' Defaults to TRUE.
#' @name store_model
#' @rdname store_model
#' @exportMethod store_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_model",
  def = function(x, base, project, overwrite = TRUE, validate = TRUE) {
    standardGeneric("store_model") # nocov
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag noNA
setMethod(
  f = "store_model",
  signature = signature(base = "character"),
  definition = function(x, base, project, overwrite = TRUE, validate = TRUE) {
    assert_that(is.flag(overwrite))
    assert_that(noNA(overwrite))
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(base))
    assert_that(file_test("-d", base))
    assert_that(is.string(project))
    assert_that(is.flag(validate))
    if (isTRUE(validate)) {
      validObject(x, complete = TRUE)
    }

    status <- status(x)
    fingerprint <- get_file_fingerprint(x)
    part <- substring(fingerprint, 1, 4)

    #create dir is it doesn't exist
    dir <- file.path(base, project, part, status) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    current <- file.path(base, project, part) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      list.files(
        pattern = sprintf("%s.rds$", fingerprint),
        full.names = TRUE, recursive = TRUE
      )
    filename <- file.path(dir, sprintf("%s.rds", fingerprint)) %>%
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
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom aws.s3 bucket_exists copy_object delete_object get_bucket
#' s3saveRDS
#' @importFrom purrr map_chr
#' @include import_s3_classes.R
setMethod(
  f = "store_model",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project, overwrite = TRUE, validate = TRUE) {
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(project))
    assert_that(is.flag(overwrite))
    assert_that(is.flag(validate))
    if (isTRUE(validate)) {
      validObject(x, complete = TRUE)
    }

    status <- status(x)
    fingerprint <- get_file_fingerprint(x)
    part <- substring(fingerprint, 1, 4)

    get_bucket(
      bucket = base,
      prefix = paste(project, part, sep = "/")
    ) %>%
      map_chr("Key") -> existing
    sprintf("%s/%s/.*/([[:xdigit:]]{40})\\.rds", project, part) %>%
      gsub("\\1", existing) -> hashes
    if (fingerprint %in% hashes) {
      if (!isTRUE(overwrite)) {
        return(fingerprint)
      } else {
        old <- existing[hashes == fingerprint]
        backup <- paste0(
          file.path(
            "abv", "backup",
            sha1(list(project, fingerprint, status, Sys.time())), sep = "/"
          )
        )
        copy_object(
          from_object = old[1], to_object = backup,
          from_bucket = base, to_bucket = base
        )
        delete_object(old, bucket = base)
      }
    }

    filename <- file.path(project, part, status, sprintf("%s.rds", fingerprint))

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
    if (fingerprint %in% hashes && isTRUE(overwrite)) {
      if (!bucket_ok) {
        copy_object(
          from_object = backup, to_object = old[1],
          from_bucket = base, to_bucket = base
        )
      }
      delete_object(backup, bucket = base)
    }
    if (!bucket_ok) {
      stop("Unable to write to S3 bucket")
    }
    return(filename)
  }
)
