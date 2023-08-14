#' @rdname get_result
#' @inheritParams read_model
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom methods setMethod new
#' @importFrom purrr map_chr
#' @include import_s3_classes.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kManifest"),
  definition = function(x, ..., base, project, verbose = TRUE) {
    assert_that(
      inherits(base, "s3_bucket"), is.string(project), noNA(project),
      validObject(x), is.flag(verbose), noNA(verbose)
    )
    manifest <- order_manifest(manifest = x)
    data.frame(
      object = manifest,
      status = map_chr(manifest, get_result_s3, base = base, project = project)
    )
  }
)

#' @importFrom assertthat assert_that
#' @importFrom aws.s3 get_bucket s3saveRDS
#' @importFrom purrr map_chr
get_result_s3 <- function(hash, base, project, verbose = TRUE) {
  display(verbose = verbose, paste(Sys.time(), hash), linefeed = FALSE)

  target <- sprintf("%s/results/%s.rds", project, hash)
  if (length(get_bucket(bucket = base, prefix = target, max = 1)) > 0) {
    display(verbose = verbose, " already done")
    return("converged")
  }
  substring(hash, 1, 4) |>
    sprintf(fmt = "%2$s/%1$s", project) |>
    get_bucket(bucket = base, max = Inf) |>
    map_chr("Key") -> available
  available <- available[grepl(hash, available)]
  if (length(available) != 1) {
    display(verbose = verbose, " object not found or multiple objects found")
    return("object problem")
  }
  substring(hash, 1, 4) |>
    sprintf(fmt = "%2$s/%1$s/(\\w+)/%3$s.rds", project, hash) |>
    gsub(replacement = "\\1", available) -> hash_status
  display(verbose = verbose, sprintf(" %s", hash_status))
  if (hash_status != "converged") {
    return(hash_status)
  }
  display(verbose = verbose, "    downloading object", FALSE)
  x <- read_model(x = hash, base = base, project = project)
  display(verbose = verbose, " done")
  x <- try(get_result(x))
  if (inherits(x, "try-error")) {
    return("get_result() failed")
  }

  # try several times to write to S3 bucket
  # avoids errors due to time out
  i <- 1
  repeat {
    bucket_ok <- tryCatch(
      s3saveRDS(x, bucket = base, object = target, multipart = TRUE),
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
  rm(x)
  gc(verbose = FALSE)
  return("converged")
}
