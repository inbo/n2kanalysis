#' @inheritParams read_result
#' @name result_estimate
#' @rdname result_estimate
#' @exportMethod result_estimate
#' @docType methods
#' @importFrom assertthat assert_that is.dir is.string noNA
#' @importFrom methods setMethod
#' @importFrom purrr list_rbind map
setMethod(
  f = "result_estimate",
  signature = signature(x = "character"),
  definition = function(x, ..., base, project) {
    if (length(x) > 1) {
      map(x, result_estimate, base = base, project = project) |>
        list_rbind() -> estimates
      return(estimates)
    }
    assert_that(is.string(x), noNA(x))
    if (grepl("[[:xdigit:]]{40}", x)) {
      read_result(x = x, base = base, project = project) |>
        result_estimate() -> estimates
      return(estimates)
    }
    assert_that(is.dir(x), is.dir(path(x, "results")))
    path(x, "results") |>
      dir_ls(type = "file", regexp = "[[:xdigit:]]{40}.rds$") |>
      basename() |>
      gsub(pattern = "\\.rds$", replacement = "") |>
      result_estimates(base = dirname(x), project = basename(x))
  }
)
