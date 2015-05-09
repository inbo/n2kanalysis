#' Fit the model to the analysis files
#' @param path the path containing the analysis files
#' @export
fit_every_model <- function(path = "."){
  path <- check_path(path, type = "directory")
  files <- list.files(path = path, pattern = "\\.rda$", full.names = TRUE)
  junk <- lapply(files, fit_model)
  return(invisible(NULL))
}
