#' Fit a Poisson GLMM model  with lme4
#' @param model A character with the covariates of the model
#' @param data The data.frame with the data
#' @param weight Name of the weights variable
#' @export
#' @importFrom lme4 glmer glmerControl
fit_glmer_poisson <- function(model, data, weight){
  if(missing(weight)){
    local.weight <- rep(1, nrow(data))
  } else {
    local.weight <- data[, weight]
  }
  
  model.formula <- as.formula(paste("Count ~", model))
  controls <- list(
    glmerControl(optimizer = "bobyqa"),
    glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
  )
  for(control in controls){
    model <- glmer(
      formula = model.formula,
      data = data,
      family = "poisson",
      weights = local.weight,
      control = control
    )
    if(length(model@optinfo$conv$lme4) == 0){
      break
    }
  }
  return(model)
}
