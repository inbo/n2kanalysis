#' Store an `n2kManifest` object
#' @param x the `n2kManifest`
#' @param base the base location to store the manifest
#' @param project will be a relative path within the base location
#' @name store_manifest
#' @rdname store_manifest
#' @exportMethod store_manifest
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_manifest",
  def = function(x, base, project) {
    standardGeneric("store_manifest") # nocov
  }
)

#' @rdname store_manifest
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom utils write.table
setMethod(
  f = "store_manifest",
  signature = signature(base = "character"),
  definition = function(x, base, project) {
    assert_that(inherits(x, "n2kManifest"))
    assert_that(is.string(base))
    assert_that(file_test("-d", base))
    assert_that(is.string(project))
    validObject(x, complete = TRUE)

    #create dir is it doesn't exist
    dir <- file.path(base, project, "manifest") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    #test if file exists
    fingerprint <- get_file_fingerprint(x)
    filename <- list.files(
        dir,
        pattern = sprintf("%s.manifest$", fingerprint),
        full.names = TRUE
      )
    if (length(filename) > 0) {
      return(normalizePath(filename, winslash = "/"))
    }
    filename <- file.path(dir, sprintf("%s.manifest", fingerprint))
    write.table(x@Manifest, file = filename, row.names = FALSE, sep = "\t")
    return(normalizePath(filename, winslash = "/"))
  }
)

#' @rdname store_manifest
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom aws.s3 bucket_exists get_bucket s3write_using
#' @importFrom utils write.table
#' @include import_s3_classes.R
setMethod(
  f = "store_manifest",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project) {
    assert_that(inherits(x, "n2kManifest"))
    assert_that(is.string(project))
    validObject(x, complete = TRUE)

    filename <- file.path(
      project, "manifest", sprintf(
        "%s.manifest",
        get_file_fingerprint(x)
      ), fsep = "/"
    )
    # check if object with same fingerprint exists
    existing <- get_bucket(base, prefix = filename)
    if (length(existing) > 0) {
      return(existing)
    }

    # create object if it doesn't exists
    # try several times to write to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bucket_ok <- tryCatch(
        s3write_using(
          x@Manifest,
          write.table,
          row.names = FALSE,
          sep = "\t",
          bucket = base,
          object = filename
        ),
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
    get_bucket(base, prefix = filename)
  }
)
