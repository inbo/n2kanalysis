#' Store to docker configuration on S3
#' @inheritParams store_manifest
#' @param docker the docker image to use
#' @param dependencies extra github package to install
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>%
#' @export
store_manifest_yaml <- function(x, base, project, docker, dependencies){
  assert_that(inherits(base, "s3_bucket"))
  assert_that(is.string(docker))
  assert_that(is.character(dependencies))

  stored <- store_manifest(x = x, base = base, project = project)
  yaml <- sprintf("  - %s", dependencies) %>%
    paste(collapse = "\n") %>%
    sprintf(
      fmt = "github:\n%s\ndocker: %s\nbucket: %s\nproject: %s\nhash: %s",
      docker,
      attr(base, "Name"),
      project,
      basename(stored$Contents$Key)
    )
  filename <- gsub("\\.manifest", ".yaml", stored$Contents$Key) %>%
    gsub(pattern = "(.*/)manifest(/.*)", replacement = "\\1yaml\\2")
  available <- get_bucket(base, prefix = filename)
  if (length(available)) {
    return(available)
  }

  # try several times to write to S3 bucket
  # avoids errors due to time out
  i <- 1
  repeat {
    bucket_ok <- tryCatch(
      s3write_using(
        yaml,
        writeLines,
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
