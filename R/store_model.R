#' Store a n2kModel object
#' @param x the n2kModel
#' @param base the base location to store the model
#' @param project will be a relative path within the base location
#' @name store_model
#' @rdname store_model
#' @exportMethod store_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_model",
  def = function(x, base, project){
    standardGeneric("store_model") # nocov
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
setMethod(
  f = "store_model",
  signature = signature(base = "character"),
  definition = function(x, base, project){
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(base))
    assert_that(file_test("-d", base))
    assert_that(is.string(project))
    validObject(x, complete = TRUE)
    status <- status(x)

    #create dir is it doesn't exist
    dir <- sprintf("%s/%s/%s", base, project, status) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    #test is file exists
    fingerprint <- get_file_fingerprint(x)
    filename <- list.files(
        dir,
        pattern = sprintf("%s.rds$", fingerprint),
        full.names = TRUE
      )
    if (length(filename) > 0) {
      return(filename)
    }

    # see if the file exists in other dirs
    current <- sprintf("%s/%s", base, project) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      list.files(
        pattern = sprintf("%s.rds$", fingerprint),
        full.names = TRUE,
        recursive = TRUE
      )
    filename <- sprintf("%s/%s.rds", dir, fingerprint) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    # store the file in the current dir and remove it from other dirs
    saveRDS(x, file = filename)
    file.remove(current)

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
  definition = function(x, base, project){
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(project))
    validObject(x, complete = TRUE)
    status <- status(x)

    # try several times to connect to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bucket_ok <- tryCatch(bucket_exists(base))
      if (is.logical(bucket_ok)) {
        break
      }
      if (i > 10) {
        stop("Unable to connect to S3 bucket")
      }
      i <- i + 1
      # waiting time between tries increases with the number of tries
      Sys.sleep(i)
    }
    if (!bucket_ok) {
      stop("Unable to connect to S3 bucket")
    }

    # check if object with same fingerprint exists
    existing <- get_bucket(base, prefix = project)
    existing <- existing[names(existing) == "Contents"] %>%
      sapply("[[", "Key")
    fingerprint <- get_file_fingerprint(x)
    current <- existing[grepl(sprintf("%s.rds$", fingerprint), existing)]
    exists <- grepl(sprintf("%s/%s.rds$", status, fingerprint), current)
    filename <- current[exists] %>%
      unname()
    if (length(filename) > 0) {
      return(filename)
    }

    # create object if it doesn't exists
    filename <- sprintf("%s/%s/%s.rds", project, status, fingerprint) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      gsub(pattern = "//", replacement = "/") %>%
      gsub(pattern = "^/", replacement = "")
    s3saveRDS(x, bucket = base, object = filename)
    if (any(!exists)) {
      delete_object(object = current[!exists], bucket = base)
    }

    return(filename)
  }
)
