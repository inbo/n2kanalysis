#' Fit the model to the analysis files
#' @param path The path containing the analysis files.
#' @param status A vector with status levels to (re-)fit the model.
#' Defaults to `c("new", "waiting")` when missing.
#' @param verbose Show the name of the current analysis file on screen.
#' Defaults to `TRUE`
#' @param n_cluster The number of clusters to use.
#' @param ... Arguments passed to [fit_model()]
#' @export
#' @importFrom n2khelper check_character
fit_every_model <- function(path, status, verbose = TRUE, n_cluster = 1, ...) {
  assert_that(is.dir(path))
  if (missing(status)) {
    status <- c("new", "waiting")
  } else {
    status <- check_character(status, name = "status")
    test_status <- status %in%
      c("new", "waiting", "error", "converged", "false_convergence", "unstable")
    if (!all(test_status)) {
      warning(
        "Following status values are ignored: ",
        paste(status[!test_status], collapse = ", ")
      )
      status <- status[test_status]
    }
  }
  files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)
  if (n_cluster == 1 || !requireNamespace("parallel", quietly = TRUE)) {
    lapply(files, fit_model, status = status, verbose = verbose, ...)
    return(invisible(NULL))
  }
  n_cluster <- min(n_cluster, parallel::detectCores())
  display(
    verbose,
    sprintf("Fitting models in parallel on %i clusters", , n_cluster)
  )
  cl <- parallel::makeCluster(n_cluster)
  parallel::clusterApplyLB(
    cl = cl,
    x = files,
    fun = function(x, status, verbose, ...) {
      require(n2kanalysis)
      fit_model(x = x, status = status, verbose = verbose, ...)
    },
    status = status,
    verbose = verbose
  )
  parallel::stopCluster(cl)

  return(invisible(NULL))
}
