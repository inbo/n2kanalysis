#' @rdname get_result
#' @importFrom methods setMethod validObject new
#' @importFrom assertthat assert_that is.string is.flag is.count noNA
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
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    # x is an existing file
    if (file_test("-f", x)) {
      if (verbose) {
        message(x)
      }
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
    if (n.cluster == 1) {
      result <- lapply(files, get_result, verbose = verbose, ...)
    } else {
      # nocov start
      if (requireNamespace("parallel", quietly = TRUE)) {
        available.cluster <- parallel::detectCores()
        if (n.cluster > available.cluster) {
          message(
            "Requesting ", n.cluster, " clusters but only ", available.cluster,
            " available."
          )
          n.cluster <- available.cluster
        }
        if (verbose) {
          message("Reading results in parallel on ", n.cluster, " clusters")
        }
        utils::flush.console()
        cl <- parallel::makeCluster(n.cluster)
        result <- parallel::clusterApplyLB(
          cl = cl,
          x = files,
          fun = get_result,
          verbose = verbose,
          ...
        )
        parallel::stopCluster(cl)
      } else {
        message(
"Cannot load the parallel package. Falling back to non-parallel computing."
        )
        utils::flush.console()
        result <- lapply(files, get_result, verbose = verbose, ...)
      }
      # nocov end
    }

    if (verbose) {
      message("Combining results")
    }
    utils::flush.console()
    result <- do.call(combine, result)

    return(result)
  }
)
