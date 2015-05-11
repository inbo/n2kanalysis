#' Fit a n2kModel object
#' @param x the n2kModel
#' @param ... other arguments. See details
#' @name fit_model
#' @rdname fit_model
#' @exportMethod fit_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "fit_model", 
  def = function(x, ...){
    standard.generic("fit_model")
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom n2khelper check_path read_object_environment
#' @details
#' \describe{
#'  \item{\code{status}}{A vector with status levels naming the levels which should be recalculated. Defaults to \code{"new"}}
#'  \item{\code{verbose}}{A logical indicating if the function should display the name of the file and the status. Defaults to \code{TRUE}}
#' }
setMethod(
  f = "fit_model",
  signature = signature(x = "character"),
  definition = function(x, ...){
    x <- check_path(x, type = "file")
    dots <- list(...)
    if(is.null(dots$verbose)){
      dots$verbose <- TRUE
    } else {
      dots$verbose <- check_single_logical(dots$verbose, name = "verbose")
    }
    if(dots$verbose){
      message(x)
      utils::flush.console()
    }
    local.environment <- new.env()
    load(x, envir = local.environment)
    analysis <- read_object_environment(object = "analysis", env = local.environment)
    analysis <- fit_model(
      x = analysis, 
      status = dots$status
    )
    if(dots$verbose){
      message(status(analysis))
      utils::flush.console()
    }
    assign("analysis", value = analysis, envir = local.environment)
    save(list = ls(local.environment), envir = local.environment, file = x)
    return(invisible(NULL))
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom n2khelper check_dataframe_covariate check_dataframe_variable
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kGlmerPoisson"),
  definition = function(x, ...){
    dots <- list(...)
    if(is.null(dots$status)){
      dots$status <- "new"
    }
    if(!(status(x) %in% dots$status)){
      return(x)
    }
    
    data <- get_data(x)
    check_dataframe_covariate(
      df = data[1, ], covariate = get_covariate(x), response = "Count", error = TRUE
    )  
    
    weight <- get_weight(x)
    if(weight == ""){
      local.weight <- rep(1, nrow(data))
    } else {
      check_dataframe_variable(df = data, variable = weight, name = "data")
      local.weight <- data[, weight]
    }
    
    model.formula <- as.formula(paste("Count ~", get_covariate(x)))
    controls <- list(
      lme4::glmerControl(optimizer = "bobyqa"),
      lme4::glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
    )
    for(control in controls){
      if("optimx" %in% control$optimizer){
        requireNamespace("optimx", quietly = TRUE)
      }
      model <- try(lme4::glmer(
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
      status(x) <- "error"
      return(x)
    }
    if(length(model@optinfo$conv$lme4) > 0){
      status(x) <- "false convergence"
      return(x)
    }
    return(n2k_glmer_poisson(data = x, model.fit = model, status = "converged"))
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom n2khelper check_dataframe_covariate
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaNbinomial"),
  definition = function(x, ...){
    dots <- list(...)
    if(is.null(dots$status)){
      dots$status <- "new"
    }
    if(!(status(x) %in% dots$status)){
      return(x)
    }
    
    if(!requireNamespace("INLA", quietly = TRUE)){
      stop("The INLA package is required but not installed.")
    }
    
    data <- get_data(x)
    check_dataframe_covariate(
      df = data[1, ], covariate = get_covariate(x), response = "Count", error = TRUE
    )  
    
    form <- as.formula(paste("Count ~", get_covariate(x)))
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
      status(x) <- "error"
      return(x)
    }
    return(n2k_inla_nbinomial(data = x, model.fit = model, status = "converged"))
  }
)
