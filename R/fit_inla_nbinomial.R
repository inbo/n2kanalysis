#' Fit an INLA model with negative binomial distribution
#' 
#' The function requires the INLA package
#' @inheritParams fit_glmer_poisson
#' @return Either an INLA model or \code{"error"} in case the fitting yields an error
#' @importFrom n2khelper check_dataframe_covariate
fit_inla_nbinomial <- function(model.set, data){
  if(!requireNamespace("INLA", quietly = TRUE)){
    stop("The INLA package is required but not installed.")
  }
  check_dataframe_covariate(
    df = data[1, ], covariate = model.set, response = "Count", error = TRUE
  )  
  
  form <- as.formula(paste("Count ~", model.set))
  link <- rep(NA, nrow(data))
  link[is.na(data$Count)] <- 1
  
  model <- try(INLA::inla(
    formula = form, 
    family = "nbinomial", 
    data = data, 
    control.compute = list(dic = TRUE, cpo = TRUE),
    control.predictor = list(compute = TRUE, link = link),
    control.fixed = list(prec.intercept = 1)
  ))
  if("try-error" %in% class(model)){
    return("error")
  }
  return(model)
}
