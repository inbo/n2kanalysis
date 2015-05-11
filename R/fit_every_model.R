#' Fit the model to the analysis files
#' @param path the path containing the analysis files
#' @param status a vector with status levels to (re-)fit the model. Defaults to "new" when missing.
#' @param verbose Show the name of the current analysis file on screen. Defaults to TRUE
#' @export
fit_every_model <- function(path = ".", status, verbose = TRUE){
  if(missing(status)){
    status <- "new"
  } else {
    status <- check_character(status, name = "status")
    test.status <- status %in% c("new", "error", "converged", "false convergence")
    if(!all(test.status)){
      warning(
        "Following status values are ignored: ", 
        paste(status[!test.status], collapse = ", ")
      )
      status <- status[test.status]
    }
  }
  path <- check_path(path, type = "directory")
  files <- list.files(path = path, pattern = "\\.rda$", full.names = TRUE)
  junk <- lapply(files, fit_model, status = status, verbose = verbose)
  return(invisible(NULL))
}
