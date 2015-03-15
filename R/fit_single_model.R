#' Run a single element from the to do list
#' @param file The name of the rda file
#' @importFrom digest digest
#' @export
fit_single_model <- function(file){
  local.environment <- new.env()
  load(file, envir = local.environment)
  species.id <- get("species.id", envir = local.environment)
  model.type <- get("model.type", envir = local.environment)
  model.set <- get("model.set", envir = local.environment)
  weighted <- exists("weight", envir = local.environment)
  output <- data.frame(
    SpeciesID = species.id,
    ModelType = model.type,
    ModelSet = model.set
  )
  if(weighted){
    weight <- get("weight", envir = local.environment)
    output$Weight <- weight
  }
  
  if(exists("completed", envir = local.environment)){
    output$Status <- get("completed", envir = local.environment)
    return(output)
  }
  data <- get("data", envir = local.environment)
  
  if(model.type == "glmer poisson"){
    if(weighted){
      model <- fit_glmer_poisson(
        model = model.set,
        data = data,
        weight = weight
      )
    } else {
      model <- fit_glmer_poisson(
        model = model.set,
        data = data
      )
    }
  }

  if("try-error" %in% class(model)){
    completed <- "error"
  } else if(is.logical(model)){
    completed <- "false convergence"
  } else {
    completed <- "converged"
  }
  output$Status <- get("completed", envir = local.environment)
  assign("completed", value = completed, envir = local.environment)
  save(list = ls(local.environment), envir = local.environment, file = file)
  
  
  if(completed != "converged"){
    return(output)
  }
  
  if(!file_test("-d", "model")){
    dir.create("model")
  }
  if(weighted){
    sha <- digest(
      list(species.id, model.type, model.set, model, weight), 
      algo = "sha1"
    )
    save(species.id, model.type, model.set, model, weight, file = paste0("model/", sha, ".rda"))
  } else {
    sha <- digest(
      list(species.id, model.type, model.set, model), 
      algo = "sha1"
    )
    save(species.id, model.type, model.set, model, file = paste0("model/", sha, ".rda"))
  }
  return(output)
}
