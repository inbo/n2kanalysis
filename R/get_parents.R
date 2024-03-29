#' Get the object of the parent analyses of an `n2kModel` object
#' @param child the child object
#' @inheritParams read_model
#' @importFrom assertthat assert_that
#' @export
get_parents <- function(child, base, project) {
  assert_that(inherits(child, "n2kModel"))
  output <- lapply(
    child@AnalysisRelation$parent_analysis,
    read_model,
    base = base,
    project = project
  )
  names(output) <- child@AnalysisRelation$parent_analysis
  return(output)
}
