#' Display a message.
#'
#' This is a short cut for `if(verbose) message(x)`.
#' @param verbose A logical.
#' When `TRUE` print the message.
#' When `FALSE` do nothing.
#' @param message a vector passed to [message()].
#' @param linefeed A logical.
#' When `TRUE` append a newline character at the end of the message.
#' @export
#' @importFrom utils flush.console
#' @importFrom assertthat assert_that is.flag noNA
display <- function(verbose, message, linefeed = TRUE) {
  assert_that(is.flag(verbose), noNA(verbose))
  assert_that(is.flag(linefeed), noNA(linefeed))

  if (verbose) {
    message(message, appendLF = linefeed)
    flush.console()
  }
  return(invisible(NULL))
}
