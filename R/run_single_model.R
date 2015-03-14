#' Run a single element from the to do list
#' @param file The name of the rda file
#' @importFrom digest digest
#' @export
run_single_model <- function(file){
  local.environment <- new.env()
  load(file, envir = local.environment)

  species.id <- get("species.id", local.environment)
  model.type <- get("model.type", local.environment)
  model.set <- get("model.set", local.environment)
  data <- get("data", local.environment)
  weighted <- exists("weight", local.environment)
  if(weighted){
    weight <- get("weight", local.environment)
  }
  
  if(model.type == "fit_glmer_poisson"){
    if(weighted){
      model <- try(fit_glmer_poisson(
        model = model.set,
        data = data,
        weight = weight
      ))
    } else {
      model <- try(fit_glmer_poisson(
        model = model.set,
        data = data
      ))
    }
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
  if("try-error" %in% class(model)){
    return("error")
  } else if(is.logical(model)){
    return("false convergence")
  } else {
    return("convergered")
  }
}
