#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom aws.s3 s3readRDS
#' @details
#' \describe{
#'  \item{\code{status}}{A vector with status levels naming the levels which should be recalculated. Defaults to \code{"new"}}
#'  \item{\code{verbose}}{A logical indicating if the function should display the name of the file and the status. Defaults to \code{TRUE}}
#' }
#' @include import_S3_classes.R
setMethod(
  f = "fit_model",
  signature = signature(x = "s3_object"),
  definition = function(x, ...){
    dots <- list(...)
    if (is.null(dots$verbose)) {
      dots$verbose <- TRUE
    } else {
      assert_that(is.flag(dots$verbose))
      assert_that(noNA(dots$verbose))
    }
    if (dots$verbose) {
      message(x$Key)
    }
    if (is.null(dots$base)) {
      dots$base <- get_bucket(x$Bucket)
    }
    if (is.null(dots$project)) {
      dots$project <- gsub(
        pattern = "(.*)/(.*)/([[:xdigit:]]{1,40})\\.(rds|manifest)$",
        replacement = "\\1",
        x$Key
      )
    }
    if (grepl("\\.manifest$", x$Key)) {
      hash <- gsub(".*?([[:xdigit:]]{1,40}).manifest$", "\\1", x$Key)
      read_manifest(base = dots$base, project = dots$project, hash = hash) %>%
        fit_model(base = dots$base, project = dots$project, ...)
      return(invisible(NULL))
    }
    analysis <- s3readRDS(object = x)
    current_status <- status(analysis)
    if (dots$verbose) {
      message(status(analysis), " -> ", appendLF = FALSE)
      utils::flush.console()
    }
    analysis.fitted <- fit_model(
      x = analysis,
      status = dots$status,
      base = dots$base,
      project = dots$project
    )
    if (dots$verbose) {
      message(status(analysis.fitted))
      utils::flush.console()
    }
    store_model(
      analysis.fitted,
      base = dots$base,
      project = dots$project
    )
    return(invisible(NULL))
  }
)
