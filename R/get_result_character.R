#' @rdname get_result
#' @importFrom methods setMethod validObject new
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom utils file_test
setMethod(
  f = "get_result",
  signature = signature(x = "character", base = "character"),
  definition = function(x, base, ..., project, verbose = TRUE) {
    # check arguments
    assert_that(
      is.string(x),
      is.string(base),
      is.string(project),
      noNA(x),
      noNA(base),
      noNA(project)
    )
    stopifnot("`base` is not a existing directory" = file_test("-d", base))

    target <- file.path(base, project, "results", sprintf("%s.rds", x))
    dirname(target) |>
      dir.create(showWarnings = FALSE, recursive = TRUE)

    # x is an existing file
    if (file_test("-f", target)) {
      display(verbose = verbose, paste("  already extracted", x))
      return(readRDS(target))
    }
    display(verbose = verbose, paste("  extracting", x))

    read_model(x = x, base = base, project = project) |>
      get_result(
        base = base,
        project = project,
        ...,
        verbose = verbose
      ) -> result
    if (status(result) == "converged") {
      saveRDS(result, file = target)
    }
    return(result)
  }
)

#' @rdname get_result
#' @importFrom methods setMethod validObject new
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom utils file_test
setMethod(
  f = "get_result",
  signature = signature(x = "character", base = "s3_bucket"),
  definition = function(x, base, ..., project, verbose = TRUE) {
    # check arguments
    assert_that(is.string(x), is.string(project), noNA(x), noNA(project))
    target <- sprintf("%s/results/%s.rds", project, x)
    result <- get_bucket(bucket = base, prefix = target, max = 1)
    if (length(result) == 1) {
      display(verbose = verbose, paste("  already extracted", x))
      return(s3readRDS(result$Contents))
    }
    stopifnot(length(result) == 0)
    display(verbose = verbose, paste("  extracting", x))
    substring(x, 1, 4) |>
      sprintf(fmt = "%2$s/%1$s", project) |>
      get_bucket(bucket = base, max = Inf) -> available
    available <- available[
      map_chr(available, "Key") |>
        grepl(pattern = x)
    ]
    stopifnot(
      "object not found or multiple objects found" = length(available) == 1
    )
    get_result(
      available[[1]],
      base = base,
      project = project,
      verbose = verbose,
      ...
    )
  }
)
