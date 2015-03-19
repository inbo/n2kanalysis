#' Fit the model to the analysis files
#' @inheritParams fit_single_model
#' @export
fit_every_model <- function(path = "."){
  if(!file_test("-d", path)){
    stop(path, " is not a directory")
  }
  files <- list.files(path = path, pattern = "\\.rda$")
  convergence <- lapply(files, fit_single_model, path = path)
  convergence <- do.call(rbind, convergence)
  if(!file_test("-d", paste0(path, "/database"))){
    dir.create(paste0(path, "/database"))
  }
  save(convergence, file = paste0(path, "/database/convergence.rda"))
  return(convergence)
}
