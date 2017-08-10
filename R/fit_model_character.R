#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_path
#' @importFrom assertthat assert_that is.flag noNA
#' @details
#' \describe{
#'  \item{\code{status}}{A vector with status levels naming the levels which should be recalculated. Defaults to \code{"new"}}
#'  \item{\code{verbose}}{A logical indicating if the function should display the name of the file and the status. Defaults to \code{TRUE}}
#' }
setMethod(
  f = "fit_model",
  signature = signature(x = "character"),
  definition = function(x, ...){
    x <- check_path(x, type = "file")
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
    if (grepl("\\.manifest$", x)) {
      hash <- gsub(".*?([[:xdigit:]]{1,40}).manifest$", "\\1", x)
      if (!"base" %in% names(dots)) {
        dots$base <- gsub("(.*)/.*?/manifest", "\\1", dirname(x))
      }
      if (!"project" %in% names(dots)) {
        dots$project <- gsub(".*/(.*?)/manifest", "\\1", dirname(x))
      }
      read_manifest(base = dots$base, project = dots$project, hash = hash) %>%
        fit_model(base = dots$base, project = dots$project, ...)
      return(invisible(NULL))
    }
    analysis <- readRDS(x)
    current_status <- status(analysis)
    base_dir <- sprintf("(.*)%s/[0-9a-f]{40}.rds", current_status) %>%
      gsub(replacement = "\\1", x = x)
    if (dots$verbose) {
      message(status(analysis), " -> ", appendLF = FALSE)
      utils::flush.console()
    }
    analysis.fitted <- fit_model(
      x = analysis,
      status = dots$status,
      path = base_dir
    )
    if (dots$verbose) {
      message(status(analysis.fitted))
      utils::flush.console()
    }
    store_model(analysis.fitted, base = base_dir, project = "")
    return(invisible(NULL))
  }
)
