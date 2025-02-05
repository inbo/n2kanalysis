#' @importFrom assertthat assert_that is.count is.flag noNA
#' @importFrom aws.s3 bucket_exists get_bucket s3write_using
#' @importFrom purrr map_chr
write_s3_fun <- function(
  object, bucket, key, fun = write.table, overwrite = FALSE, opts = NULL, ...,
  max_attempt = 10
) {
  assert_that(is.flag(overwrite), noNA(overwrite), is.count(max_attempt))
  # check if object with same fingerprint exists in case we don't overwrite
  existing <- get_bucket(bucket, prefix = key)
  if (!overwrite && length(existing) > 0) {
    return(unname(map_chr(existing, "Key")))
  }

  # create object if it doesn't exists or we want to overwrite
  # try several times to write to S3 bucket
  # avoids errors due to time out
  i <- 1
  repeat {
    bucket_ok <- tryCatch(
      s3write_using(
        x = object, FUN = fun, bucket = bucket, object = key, opts = opts, ...
      ),
      error = function(err) {
        err
      }
    )
    if (is.logical(bucket_ok)) {
      break
    }
    stopifnot("Unable to write to S3 bucket" = i <= max_attempt)
    message("attempt ", i, " to write to S3 bucket failed. Trying again...")
    i <- i + 1
    # waiting time between tries increases with the number of tries
    Sys.sleep(i)
  }
  stopifnot("Unable to write to S3 bucket" = bucket_ok)
  get_bucket(bucket, prefix = key) |>
    map_chr("Key") |>
    unname()
}
