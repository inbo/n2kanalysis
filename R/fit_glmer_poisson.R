#' Fit a Poisson GLMM model  with lme4
#' @param model A character with the covariates of the model
#' @param data The data.frame with the data
#' @param weight Name of the weights variable
#' @export
#' @importFrom lme4 glmer glmerControl
#' @importFrom n2khelper check_single_character check_dataframe_variable check_dataframe_covariate
fit_glmer_poisson <- function(model, data, weight){
  check_dataframe_covariate(
    df = data[1, ], covariate = model, response = "Count", error = TRUE
  )  
  if(missing(weight)){
    local.weight <- rep(1, nrow(data))
  } else {
    check_dataframe_variable(df = data, variable = weight, name = "data")
    local.weight <- data[, weight]
  }
  
  model.formula <- as.formula(paste("Count ~", model))
  controls <- list(
    glmerControl(optimizer = "bobyqa"),
    glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
  )
  for(control in controls){
    if("optimx" %in% control$optimizer){
      requireNamespace("optimx", quietly = TRUE)
    }
    model <- try(glmer(
      formula = model.formula,
      data = data,
      family = "poisson",
      weights = local.weight,
      control = control
    ))
    if("try-error" %in% class(model)){
      next
    }
    if(length(model@optinfo$conv$lme4) == 0){
      break
    }
  }
  if("try-error" %in% class(model)){
    return("error")
  }
  if(length(model@optinfo$conv$lme4) > 0){
    return("false convergence")
  }
  return(model)
}
