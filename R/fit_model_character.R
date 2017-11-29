#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom aws.s3 get_bucket
#' @details
#' \describe{
#'  \item{\code{status}}{A vector with status levels naming the levels which should be recalculated. Defaults to \code{"new"}}
#'  \item{\code{verbose}}{A logical indicating if the function should display the name of the file and the status. Defaults to \code{TRUE}}
#' }
setMethod(
  f = "fit_model",
  signature = signature(x = "character"),
  definition = function(x, ...){
    assert_that(is.string(x))
    dots <- list(...)
    if (is.null(dots$verbose)) {
      dots$verbose <- TRUE
    } else {
      assert_that(is.flag(dots$verbose))
      assert_that(noNA(dots$verbose))
    }
    if (dots$verbose) {
      message(x)
    }
    if (is.null(dots$base)) {
      if (is.null(dots$bucket)) {
        dots$base <- gsub(
          pattern = "(.*)/(.*)/.*/[[:xdigit:]]{40}\\.(rds|manifest)",
          replacement = "\\1",
          x = x
        )
      } else {
        dots$base <- get_bucket(dots$bucket)
      }
    }
    if (is.null(dots$project)) {
      dots$project <- gsub(
        pattern = "(.*)/(.*)/.*/[[:xdigit:]]{40}\\.(rds|manifest)",
        replacement = "\\2",
        x = x
      )
    }
    hash <- gsub(".*/([[:xdigit:]]{40})\\.(rds|manifest)", "\\1", x)
    if (grepl("\\.manifest$", x)) {
      read_manifest(hash, base = dots$base, project = dots$project) %>%
        fit_model(base = dots$base, project = dots$project, ...)
      return(invisible(NULL))
    }
    analysis <- read_model(hash, base = dots$base, project = dots$project)
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
    store_model(analysis.fitted, base = dots$base, project = dots$project)
    return(invisible(NULL))
  }
)
