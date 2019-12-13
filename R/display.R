#' @noRd
#' @importFrom utils flush.console
#' @importFrom assertthat assert_that is.flag noNA
display <- function(verbose, message, linefeed = TRUE) {
  assert_that(is.flag(verbose))
  assert_that(noNA(verbose))
  if (verbose) {
    message(message, appendLF = linefeed)
    flush.console()
  }
}
