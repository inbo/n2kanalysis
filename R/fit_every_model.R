#' Fit the model to the analysis files
#' @param path the path containing the analysis files
#' @param status a vector with status levels to (re-)fit the model. Defaults to c("new", "waiting") when missing.
#' @param verbose Show the name of the current analysis file on screen. Defaults to TRUE
#' @param n.cluster the number of clusters to use
#' @export
#' @importFrom n2khelper check_character check_path
fit_every_model <- function(path = ".", status, verbose = TRUE, n.cluster = 1){
  if (missing(status)) {
    status <- c("new", "waiting")
  } else {
    status <- check_character(status, name = "status")
    test.status <- status %in%
      c("new", "waiting", "error", "converged", "false convergence", "unstable")
    if (!all(test.status)) {
      warning(
        "Following status values are ignored: ",
        paste(status[!test.status], collapse = ", ")
      )
      status <- status[test.status]
    }
  }
  path <- check_path(path, type = "directory")
  files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)
  if (n.cluster == 1) {
    lapply(files, fit_model, status = status, verbose = verbose)
  } else {
    if (requireNamespace("parallel", quietly = TRUE)) {
      available.cluster <- parallel::detectCores()
      if (n.cluster > available.cluster) {
        message(
          "Requesting ", n.cluster, " clusters but only ", available.cluster,
          " available."
        )
        n.cluster <- available.cluster
      }
      message("Fitting models in parallel on ", n.cluster, " clusters")
      utils::flush.console()
      cl <- parallel::makeCluster(n.cluster)
      result <- parallel::clusterApplyLB(
        cl = cl,
        x = files,
        fun = function(x, status, verbose){
          require(optimx)
          require(n2kanalysis)
          fit_model(x = x, status = status, verbose = verbose)
        },
        status = status,
        verbose = verbose
      )
      parallel::stopCluster(cl)
    } else {
      message(
"Cannot load the parallel package. Falling back to non-parallel computing."
      )
      utils::flush.console()
      lapply(files, fit_model, status = status, verbose = verbose)
    }
  }

  return(invisible(NULL))
}
