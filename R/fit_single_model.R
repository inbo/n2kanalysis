#' Run a single element from the to do list
#' @param file The name of the rda file
#' @param path The path of the analysis files
#' @importFrom n2khelper read_object_environment
#' @importFrom digest digest
#' @export
fit_single_model <- function(file, path){
  local.file <- paste(path, file, sep = "/")
  local.environment <- new.env()
  load(local.file, envir = local.environment)
  
  species.id <- read_object_environment(object = "species.id", env = local.environment)
  region <- read_object_environment(object = "region", env = local.environment, warn = FALSE)
  model.type <- read_object_environment(object = "model.type", env = local.environment)
  model.set <- read_object_environment(object = "model.set", env = local.environment)
  weight <- read_object_environment(object = "weight", env = local.environment, warn = FALSE)
  completed <- read_object_environment(
    object = "completed", env = local.environment, warn = FALSE
  )
  
  output <- data.frame(
    SpeciesID = species.id,
    Region = ifelse(is.null(region), NA, region),
    ModelType = model.type,
    ModelSet = model.set,
    Weight = ifelse(is.null(weight), NA, weight)
  )
  
  if(is.null(completed)){
    output$Status <- completed
    return(output)
  }
  data <- read_object_environment(object = "data", en = local.environment)
  
  if(model.type == "glmer poisson"){
    model <- fit_glmer_poisson(
      model = model.set,
      data = data,
      weight = weight
    )
  }

  if("try-error" %in% class(model)){
    completed <- "error"
  } else if(is.logical(model)){
    completed <- "false convergence"
  } else {
    completed <- "converged"
  }
  output$Status <- completed
  assign("completed", value = completed, envir = local.environment)
  save(list = ls(local.environment), envir = local.environment, file = local.file)
  
  if(completed != "converged"){
    return(output)
  }
  
  if(!file_test("-d", paste0(path, "/model"))){
    dir.create(paste0(path, "/model"))
  }
  sha <- digest(
    list(
      species.id, region, model.type, model.set, model, weight
    ), 
    algo = "sha1"
  )
  save(
      species.id, region, model.type, model.set, model, weight, 
    file = paste0(path, "/model/", sha, ".rda")
  )
  return(output)
}
