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
    dir <- file.path(base, project, "manifest") |>
      normalizePath(winslash = "/", mustWork = FALSE)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    #test if file exists
    fingerprint <- get_file_fingerprint(x)
    filename <- list.files(
        dir, pattern = sprintf("%s.manifest$", fingerprint), full.names = TRUE
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
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom utils write.table
#' @include import_s3_classes.R
setMethod(
  f = "store_manifest",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project) {
    assert_that(inherits(x, "n2kManifest"), is.string(project), noNA(project))
    validObject(x, complete = TRUE)

    filename <- file.path(
      fsep = "/", project, "manifest",
      sprintf("%s.manifest", get_file_fingerprint(x))
    )
    write_s3_fun(
      object = x@Manifest, bucket = base, key = filename, overwrite = FALSE,
      row.names = FALSE, sep = "\t"
    )
  }
)
