#' @rdname get_result
#' @importFrom methods setMethod validObject new
#' @importFrom assertthat assert_that is.string is.count
#' @importFrom utils file_test
#' @param n.cluster The number of clusters to run this function in parallel.
#' Defaults to `1` (= no parallel computing).
setMethod(
  f = "get_result",
  signature = signature(x = "character"),
  definition = function(
    x,
    n.cluster = 1,
    verbose = TRUE,
    ...
  ) {
    # check arguments
    assert_that(is.string(x))
    assert_that(is.count(n.cluster))

    # x is an existing file
    if (file_test("-f", x)) {
      display(verbose, x)
      return(get_result(x = readRDS(x), verbose = verbose, ...))
    }

    if (!file_test("-d", x)) {
      stop("'x' is neither an existing file, neither an existing directory")
    }

    # x is an existing directory
    x <- normalizePath(x, winslash = "/", mustWork = TRUE)
    files <- list.files(
      path = x,
      pattern = "\\.rds$",
      full.names = TRUE,
      recursive = TRUE
    )
    if (length(files) == 0) {
      return(new("n2kResult"))
    }
    if (n.cluster == 1 || !requireNamespace("parallel", quietly = TRUE)) {
      result <- lapply(files, get_result, verbose = verbose, ...)
    } else {
      n.cluster <- min(n.cluster, parallel::detectCores())
      display(
        verbose,
        paste("Reading results in parallel on", n.cluster, "clusters")
      )
      cl <- parallel::makeCluster(n.cluster)
      result <- parallel::clusterApplyLB(
        cl = cl,
        x = files,
        fun = get_result,
        verbose = verbose,
        ...
      )
      parallel::stopCluster(cl)
    }

    display(verbose, "Combining results")
    result <- do.call(combine, result)

    return(result)
  }
)
