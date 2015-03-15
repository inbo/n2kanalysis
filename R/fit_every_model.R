#' Fit the model to the analysis files
#' @param path The path of the analysis files
#' @export
fit_every_model <- function(path){
  files <- list.files(path = path, pattern = "\\.rda$")
  convergence <- lapply(files, fit_single_model)
  convergence <- do.call(rbind, convergence)
  if(!file_test("-d", "database")){
    dir.create("database")
  }
  save(convergence, file = "database/convergence.rda")
  return(convergence)
}
