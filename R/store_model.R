#' Store an `n2kModel` object
#' @param x The `n2kModel`.
#' @param base The base location to store the model.
#' @param project Will be a relative path within the base location.
#' @param overwrite Should an existing object be overwritten?
#' Defaults to `TRUE`.
#' @param validate Check that the object is valid before storing it.
#' Defaults to `TRUE`.
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
#' @importFrom fs dir_create dir_exists dir_ls file_delete path path_abs
setMethod(
  f = "store_model",
  signature = signature(base = "character"),
  definition = function(x, base, project, overwrite = TRUE, validate = TRUE) {
    assert_that(
      is.flag(overwrite), noNA(overwrite), inherits(x, "n2kModel"),
      is.string(base), is.string(project), noNA(project), is.flag(validate)
    )
    assert_that(
      dir_exists(base), msg = sprintf("`%s` is not an existing directory", base)
    )
    base <- path_abs(base)
    if (isTRUE(validate)) {
      validObject(x, complete = TRUE)
    }

    status <- status(x)
    fingerprint <- get_file_fingerprint(x)
    part <- substring(fingerprint, 1, 4)

    base <- path(base, project, part)
    dir_create(base)
    current <- dir_ls(
      base, recurse = TRUE, type = "file",
      regexp = sprintf("%s.rds$", fingerprint)
    )
    if (length(current) > 0) {
      if (!overwrite) {
        return(current)
      }
      file_delete(current)
    }

    filename <- path(base, status, sprintf("%s.rds", fingerprint))
    dirname(filename) |>
      dir_create()
    saveRDS(x, file = filename)
    return(filename)
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom aws.s3 bucket_exists copy_object delete_object get_bucket
#' s3saveRDS
#' @importFrom purrr map_chr
#' @include import_s3_classes.R
setMethod(
  f = "store_model",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project, overwrite = TRUE, validate = TRUE) {
    assert_that(
      inherits(x, "n2kModel"), is.string(project), is.flag(overwrite),
      is.flag(validate), noNA(project), noNA(overwrite), noNA(validate)
    )
    if (isTRUE(validate)) {
      validObject(x, complete = TRUE)
    }

    status <- status(x)
    fingerprint <- get_file_fingerprint(x)
    part <- substring(fingerprint, 1, 4)

    # try several times to write to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bk <- tryCatch(
        get_bucket(bucket = base, prefix = paste(project, part, sep = "/")),
        error = function(err) {
          err
        }
      )
      if (inherits(bk, "s3_bucket")) {
        break
      }
      stopifnot("Unable to get S3 bucket" = i <= 10)
      message("attempt ", i, " to read S3 bucket failed. Trying again...")
      i <- i + 1
      # waiting time between tries increases with the number of tries
      Sys.sleep(i)
    }
    existing <- map_chr(bk, "Key")
    sprintf("%s/%s/.*/([[:xdigit:]]{40})\\.rds", project, part) |>
      gsub("\\1", existing) -> hashes
    # what to do with an existing object
    if (fingerprint %in% hashes) {
      # we are done when overwrite = FALSE
      if (!isTRUE(overwrite)) {
        return(fingerprint)
      }
      # make a backup of the existing object when overwrite = TRUE
      # then delete the original one
      old <- existing[hashes == fingerprint]
      list(project, fingerprint, status, Sys.time()) |>
        sha1() |>
        sprintf(fmt = "%2$s/backup/%1$s", project) -> backup
      copy_object(
        from_object = old[1], to_object = backup,
        from_bucket = base, to_bucket = base
      )
      delete_object(old, bucket = base)
    }

    filename <- file.path(project, part, status, sprintf("%s.rds", fingerprint))
    # try several times to write to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bucket_ok <- tryCatch(
        s3saveRDS(x, bucket = base, object = filename, multipart = TRUE),
        error = function(err) {
          err
        }
      )
      if (is.logical(bucket_ok)) {
        break
      }
      stopifnot("Unable to write to S3 bucket" = i <= 10)
      message("attempt ", i, " to write to S3 bucket failed. Trying again...")
      i <- i + 1
      # waiting time between tries increases with the number of tries
      Sys.sleep(i)
    }
    # clean up the eventual backup
    if (fingerprint %in% hashes && isTRUE(overwrite)) {
      # restore the backup because s3saveRDS() failed
      if (!bucket_ok) {
        copy_object(
          from_object = backup, to_object = old[1],
          from_bucket = base, to_bucket = base
        )
      }
      # remove the backup
      delete_object(backup, bucket = base)
    }
    # return an error when writing failed
    stopifnot("Unable to write to S3 bucket" = bucket_ok)
    return(filename)
  }
)
