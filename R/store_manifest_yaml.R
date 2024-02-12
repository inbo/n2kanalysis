#' Store a Docker configuration
#' @inheritParams store_manifest
#' @param docker the docker image to use
#' @param dependencies extra GitHub packages to install
#' @name store_manifest_yaml
#' @rdname store_manifest_yaml
#' @exportMethod store_manifest_yaml
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_manifest_yaml",
  def = function(x, base, project, docker, dependencies) {
    standardGeneric("store_manifest_yaml") # nocov
  }
)

#' @export
#' @rdname store_manifest_yaml
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>%
#' @importFrom purrr map_chr
#' @importFrom yaml write_yaml
setMethod(
  f = "store_manifest_yaml",
  signature = signature(base = "s3_bucket"),
  definition = function(x, base, project, docker, dependencies) {
    assert_that(is.string(docker))
    assert_that(is.character(dependencies))

    stored <- store_manifest(x = x, base = base, project = project)
    list(
      github = dependencies, docker = docker, bucket = attr(base, "Name"),
      project = project,
      hash = basename(stored$Contents$Key) |>
        gsub(pattern = "\\.manifest", replacement = "")
    ) -> yaml
    filename <- sprintf("%s/yaml/%s.yaml", project, sha1(yaml))
    available <- get_bucket(base, prefix = filename, max = Inf)
    if (length(available)) {
      return(map_chr(available, "Key"))
    }

    # try several times to write to S3 bucket
    # avoids errors due to time out
    i <- 1
    repeat {
      bucket_ok <- tryCatch(
        s3write_using(yaml, write_yaml, bucket = base, object = filename),
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
    stopifnot("Unable to write to S3 bucket" = bucket_ok)
    get_bucket(base, prefix = filename, max = Inf) |>
      map_chr("Key")
  }
)

#' @export
#' @rdname store_manifest_yaml
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.dir
#' @importFrom dplyr %>%
#' @importFrom yaml write_yaml
setMethod(
  f = "store_manifest_yaml",
  signature = signature(base = "character"),
  definition = function(x, base, project, docker, dependencies) {
    assert_that(is.dir(base), is.string(docker), is.character(dependencies))

    stored <- store_manifest(x = x, base = base, project = project)
    list(
      github = dependencies, docker = docker, bucket = base, project = project,
      hash = basename(stored) |>
        gsub(pattern = "\\.manifest", replacement = "")
    ) -> yaml
    sprintf("%s/%s/yaml/%s.yaml", base, project, sha1(yaml)) |>
      normalizePath(winslash = "/", mustWork = FALSE) -> filename
    if (file.exists(filename)) {
      return(filename)
    }
    if (!dir.exists(dirname(filename))) {
      dir.create(dirname(filename), recursive = TRUE)
    }
    write_yaml(yaml, filename)
    return(filename)
  }
)
