#' Store a n2kModel object
#' @param x the n2kModel
#' @param base the base location to store the model
#' @param root the path relative to the base location
#' @param path the path relative to the root location
#' @name store_model
#' @rdname store_model
#' @exportMethod store_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_model",
  def = function(x, base, root, path){
    standardGeneric("store_model") # nocov
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
setMethod(
  f = "store_model",
  signature = signature(base = "character"),
  definition = function(x, base, root, path){
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(base))
    assert_that(file_test("-d", base))
    assert_that(is.string(root))
    assert_that(is.string(path))
    validObject(x, complete = TRUE)
    fingerprint <- get_file_fingerprint(x)
    filename <- sprintf("%s/%s/%s", base, root, path) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      list.files(pattern = sprintf("%s.rds$", fingerprint))
    if (length(filename) > 0) {
      return(filename)
    }

    filename <- sprintf(
      "%s/%s/%s/%s.rds",
      base,
      root,
      path,
      fingerprint
    ) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    saveRDS(x, file = filename)
    return(filename)
  }
)

#' @rdname store_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom aws.s3 bucket_exists get_bucket s3saveRDS
setMethod(
  f = "store_model",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, root, path){
    assert_that(inherits(x, "n2kModel"))
    assert_that(is.string(root))
    assert_that(is.string(path))
    validObject(x, complete = TRUE)

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
      Sys.sleep(i)
    }

    # check if object with same fingerprint exists
    existing <- get_bucket(base, prefix = root)
    existing <- existing[names(existing) == "Contents"] %>%
      sapply("[[", "Key")
    fingerprint <- get_file_fingerprint(x)
    filename <- existing[grepl(sprintf("%s.rds$", fingerprint), existing)]
    if (length(filename) > 0) {
      return(filename)
    }

    # create object if it doesn't exists
    filename <- sprintf("%s/%s/%s.rds", root, path, fingerprint) %>%
      normalizePath(winslash = "/", mustWork = FALSE) %>%
      gsub(pattern = "//", replacement = "/") %>%
      gsub(pattern = "^/", replacement = "")
    s3saveRDS(x, bucket = base, object = filename)
    return(filename)
  }
)
