#' Read a n2kManifest object
#' @param base the base location to read the manifest
#' @param project will be a relative path within the base location
#' @param hash optinaly the sha1 of the manifest. This can be abbreviated to to first unique characters. The function will return an error in case of multiple matches. If missing, then most recent manifest will be returned.
#' @name read_manifest
#' @rdname read_manifest
#' @exportMethod read_manifest
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "read_manifest",
  def = function(base, project, hash){
    standardGeneric("read_manifest") # nocov
  }
)

#' @rdname read_manifest
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom utils read.table
#' @importFrom dplyr %>% arrange_ slice_ desc
#' @importFrom tibble rownames_to_column
setMethod(
  f = "read_manifest",
  signature = signature(base = "character"),
  definition = function(base, project, hash){
    assert_that(is.string(base))
    assert_that(file_test("-d", base))
    assert_that(is.string(project))

    #check dir if exists
    dir <- sprintf("%s/%s/manifest", base, project) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    assert_that(
      dir.exists(dir),
      msg = sprintf("No manifest files in '%s'", dir)
    )

    available <- list.files(
      dir,
      pattern = "\\.manifest$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(available) == 0) {
      stop("No manifest files in '", dir, "'")
    }

    if (missing(hash)) {
      manifest <- file.info(available) %>%
        rownames_to_column("filename") %>%
        arrange_(~desc(mtime)) %>%
        slice_(1) %>%
        "[["("filename") %>%
        read.table(
          header = TRUE,
          sep = "\t",
          colClasses = "character",
          as.is = TRUE
        ) %>%
        n2k_manifest()
      return(manifest)
    }

    assert_that(is.string(hash))

    selection <- grep(sprintf("/manifest/%s.*\\.manifest$", hash), available)
    if (length(selection) == 0) {
      stop("No manifest found starting with '", hash, "'")
    }
    if (length(selection) > 1) {
      stop("Multiple manifests found starting with '", hash, "'")
    }
    available[selection] %>%
      read.table(
        header = TRUE,
        sep = "\t",
        colClasses = "character",
        as.is = TRUE
      ) %>%
      n2k_manifest()
  }
)

#' @rdname read_manifest
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom aws.s3 bucket_exists get_bucket s3read_using
#' @importFrom dplyr %>%
#' @importFrom utils read.table
#' @include import_S3_classes.R
setMethod(
  f = "read_manifest",
  signature = signature(base = "s3_bucket"),
  definition = function(base, project, hash){
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

    if (missing(hash)) {
      available <- get_bucket(
        base,
        prefix = paste0(project, "/manifest"),
        max = Inf
      )
      if (length(available) == 0) {
        stop("No manifest files in this project")
      }
      latest <- sapply(available, function(x){x$LastModified}) %>%
        order() %>%
        which.max()
      manifest <- s3read_using(
        read.table,
        header = TRUE,
        sep = "\t",
        colClasses = "character",
        as.is = TRUE,
        object = available[[latest]]
      ) %>%
        n2k_manifest()
      return(manifest)
    }

    assert_that(is.string(hash))
    available <- get_bucket(
      base,
      prefix = paste0(project, "/manifest/", hash),
      max = Inf
    )
    if (length(available) == 0) {
      stop("No manifest found starting with '", hash, "'")
    }
    if (length(available) > 1) {
      stop("Multiple manifests found starting with '", hash, "'")
    }
    s3read_using(
      read.table,
      header = TRUE,
      sep = "\t",
      colClasses = "character",
      as.is = TRUE,
      object = available[[1]]
    ) %>%
      n2k_manifest()
  }
)
