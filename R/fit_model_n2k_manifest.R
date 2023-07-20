#' @rdname fit_model
#' @importFrom assertthat assert_that is.string is.flag is.dir has_name noNA
#' @importFrom fs dir_create dir_ls path
#' @importFrom methods setMethod new
#' @importFrom purrr walk
#' @importFrom stats na.omit
#' @include n2k_manifest_class.R
#' @param local A local folder into which objects from an AWS S3 bucket are
#' downloaded.
#' @param first A logical.
#' `first = TRUE` implies to fit only the first object in the manifest with
#' matching status.
#' `first = FALSE`  implies to fit all objects in the manifest with matching
#' status.
#' Defaults to `FALSE`.
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kManifest"),
  definition = function(
    x, base, project, status = c("new", "waiting"), verbose = TRUE, ...,
    local = tempfile("fit_model"), first = FALSE
  ) {
    assert_that(
      is.string(project), noNA(project), is.character(status), noNA(status),
      length(status) >= 1
    )
    to_do <- order_manifest(x)
    remaining <- length(to_do)
    while (length(to_do) > 1 && first) {
      head(to_do, 1) |>
        hash_status(base = base, project = project) -> stat
      if (stat %in% status) {
        to_do <- head(to_do, 1)
      } else {
        to_do <- tail(to_do, -1)
      }
    }
    if (length(to_do) == 0) {
      return(invisible(0))
    }
    if (inherits(base, "character")) {
      walk(
        to_do, fit_model, base = base, project = project,
        status = status, verbose = verbose, ...
      )
      return(invisible(remaining))
    }

    display(verbose, "Downloading objects")
    x@Manifest$parent[x@Manifest$fingerprint %in% to_do] |>
      c(to_do) |>
      unique() |>
      na.omit() -> to_download
    path(local, project) |>
      dir_create()
    path(local, project) |>
      dir_ls(recurse = TRUE, type = "file") |>
      basename() -> local_files
    to_download[!paste0(to_download, ".rds") %in% local_files] |>
      walk(
        download_model, base = base, project = project, local = local,
        verbose = verbose
      )
    walk(
      to_do, fit_model, base = local, project = project,
      status = status, verbose = verbose, ...
    )
    display(verbose, "Uploading objects")
    walk(
      to_do, download_model, base = local, project = project, local = base,
      verbose = verbose
    )
    return(invisible(remaining))
  }
)

#' @importFrom aws.s3 get_bucket
#' @importFrom purrr map_chr
hash_status <- function(hash, base, project) {
  if (inherits(base, "s3_bucket")) {
    substr(hash, 1, 4) |>
      sprintf(fmt = "%2$s/%1$s/", project) |>
      get_bucket(bucket = base, max = Inf) |>
      map_chr("Key") -> keys
    keys[grepl(hash, keys)] |>
      gsub(pattern = sprintf(".*/(.*)/%s\\.rds", hash), replacement = "\\1") |>
      unname() -> output
    return(output)
  }
  stop("hash status for ", class(base), " still do to")
}

download_model <- function(hash, base, local, project, verbose = FALSE) {
  display(verbose, paste("Moving", hash))
  read_model(x = hash, base = base, project = project) |>
    store_model(base = local, project = project)
}
