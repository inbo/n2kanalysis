#' Read a `n2kManifest` object
#' @param base The base location to read the manifest.
#' @param project Will be a relative path within the base location.
#' @param hash Optional the `sha1` of the manifest.
#' This can be abbreviated to to first unique characters.
#' The function will return an error in case of multiple matches.
#' If missing, then most recent manifest will be returned.
#' @name read_manifest
#' @rdname read_manifest
#' @exportMethod read_manifest
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "read_manifest",
  def = function(base, project, hash) {
    standardGeneric("read_manifest") # nocov
  }
)

#' @rdname read_manifest
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom utils read.table
#' @importFrom dplyr %>% arrange slice desc
#' @importFrom tibble rownames_to_column
setMethod(
  f = "read_manifest",
  signature = signature(base = "character"),
  definition = function(base, project, hash) {
    assert_that(is.string(base), file_test("-d", base), is.string(project))

    #check dir if exists
    dir <- file.path(base, project, "manifest") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    assert_that(
      dir.exists(dir),
      msg = sprintf("No manifest files in '%s'", dir)
    )

    available <- list.files(
      dir, pattern = "\\.manifest$", full.names = TRUE, ignore.case = TRUE
    )
    assert_that(
      length(available) > 0, msg = paste0("No manifest files in '", dir, "'")
    )

    if (missing(hash)) {
      file.info(available) |>
        rownames_to_column("filename") |>
        slice_max(.data$mtime, n = 1) -> latest
      read.table(
        latest$filename, header = TRUE, sep = "\t", colClasses = "character",
        as.is = TRUE
      ) |>
        n2k_manifest() -> manifest
      stopifnot(
        "fingerprint doesn't match" =
          paste0(manifest@Fingerprint, ".manifest") == basename(latest$filename)
      )
      return(manifest)
    }

    assert_that(is.string(hash))

    selection <- grep(sprintf("manifest/%s.*\\.manifest$", hash), available)
    assert_that(
      length(selection) > 0,
      msg = paste0("No manifest found starting with '", hash, "'")
    )
    assert_that(
      length(selection) == 1,
      msg = paste0("Multiple manifests found starting with '", hash, "'")
    )
    read.table(
      available[selection], header = TRUE, sep = "\t", colClasses = "character",
      as.is = TRUE
    ) |>
      n2k_manifest() -> manifest
    stopifnot("fingerprint doesn't match" = manifest@Fingerprint == hash)
    return(manifest)
  }
)

#' @rdname read_manifest
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom aws.s3 bucket_exists get_bucket s3read_using
#' @importFrom purrr map_chr
#' @importFrom utils read.table
#' @include import_s3_classes.R
setMethod(
  f = "read_manifest",
  signature = signature(base = "s3_bucket"),
  definition = function(base, project, hash) {
    if (missing(hash)) {
      assert_that(is.string(project))
      available <- get_bucket(
        base, prefix = paste(project, "manifest", sep = "/"), max = Inf
      )
      stopifnot("No manifest files in this project" = length(available) > 0)
      map_chr(available, "LastModified") |>
        as.POSIXct(format = "%Y-%m-%dT%H:%M:%OS") |>
        which.max() -> latest
      s3read_using(
        read.table, header = TRUE, sep = "\t", colClasses = "character",
        as.is = TRUE, object = available[[latest]]
      ) |>
        n2k_manifest() -> manifest
      stopifnot(
        "fingerprint doesn't match" =
          paste0(manifest@Fingerprint, ".manifest") ==
          basename(available[[latest]][["Key"]])
      )
      return(manifest)
    }

    assert_that(is.string(hash))
    if (missing(project)) {
      project <- gsub("(.*?)/manifest/.*", "\\1", hash)
      gsub(".*?/manifest/(.*)", "\\1", hash) |>
        gsub(pattern = "\\.manifest", replacement = "", x = _) -> hash
    }
    available <- get_bucket(
      bucket = base, prefix = paste(project, "manifest", hash, sep = "/")
    )
    assert_that(
      length(available) > 0,
      msg = sprintf("No manifest found starting with '%s'", hash)
    )
    assert_that(
      length(available) == 1,
      msg = sprintf("Multiple manifests found starting with '%s'", hash)
    )
    s3read_using(
      read.table, header = TRUE, sep = "\t", colClasses = "character",
      as.is = TRUE, object = available[[1]]
    ) |>
      n2k_manifest() -> manifest
    stopifnot("fingerprint doesn't match" = manifest@Fingerprint == hash)
    return(manifest)
  }
)
