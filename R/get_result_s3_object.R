#' @rdname get_result
#' @importFrom methods setMethod new
#' @include import_s3_classes.R
setMethod(
  f = "get_result",
  signature = signature(x = "s3_object"),
  definition = function(x, base, ..., project, verbose = TRUE) {
    x <- s3readRDS(object = x)
    result <- get_result(x, ..., project = project, verbose = verbose)
    if (status(result) == "converged") {
      target <- sprintf("%s/results/%s.rds", project, get_file_fingerprint(x))
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
    }
    rm(x)
    gc(verbose = FALSE)
    return(result)
  }
)
