#' Add the model parameters from a model
#' @param analysis The model to add
#' @name get_model_parameter
#' @rdname get_model_parameter
#' @exportMethod get_model_parameter
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model_parameter", 
  def = function(analysis){
    standard.generic("get_model_parameter")
  }
)

#' @rdname get_model_parameter
#' @aliases get_model_parameter,n2kGlmerPoisson-methods
#' @importFrom methods setMethod
#' @importFrom digest digest
#' @importFrom lme4 ranef VarCorr
#' @include n2kGlmerPoisson_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(analysis){
    if(analysis@Status != "converged"){
      return(new("n2kParameter"))
    }
    parameter <- data.frame(
      Description = c("Fixed effect", "Random effect BLUP", "Random effect variance"),
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, digest, algo = "sha1")
    
    # add fixed effect parameters
    variable <- c("\\(Intercept\\)", attr(attr(analysis@Model@frame, "terms"), "term.labels"))
    fixed.effect <- summary(analysis@Model)$coefficients
    parameter.estimate <- data.frame(
      Analysis = analysis@FileFingerprint,
      Parameter = row.names(fixed.effect),    
      Estimate = fixed.effect[, "Estimate"],
      LowerConfidenceLimit = qnorm(0.025, mean = fixed.effect[, "Estimate"], sd = fixed.effect[, "Std. Error"]),
      UpperConfidenceLimit = qnorm(0.975, mean = fixed.effect[, "Estimate"], sd = fixed.effect[, "Std. Error"]),
      stringsAsFactors = FALSE
    )
    row.names(parameter.estimate) <- NULL
    fixed.parent <- parameter$Fingerprint[parameter$Description == 'Fixed effect']
    for(i in variable){
      present <- grep(paste0("^", i), parameter.estimate$Parameter)
      if(length(present) == 0){
        next
      }
      extra <- data.frame(
        Description = gsub("(\\\\|\\(|\\))", "", i),
        Parent = fixed.parent,
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
      parameter <- rbind(parameter, extra)
      continuous <- grep(paste0("^", i, "$"), parameter.estimate$Parameter)
      if(length(continuous) == 1){
        parameter.estimate$Parameter[continuous] <- extra$Fingerprint
        next
      }
      extra.factor <- data.frame(
        Description = gsub(paste0("^", i), "" , parameter.estimate$Parameter[present]),
        Parent = extra$Fingerprint,
        stringsAsFactors = FALSE
      )
      extra.factor$Fingerprint <- apply(extra.factor, 1, digest, algo = "sha1")
      parameter <- rbind(parameter, extra.factor)
      parameter.estimate$Parameter[present] <- extra.factor$Fingerprint
    }
    
    # add random effect variance
    random.variance <- VarCorr(analysis@Model)
    if(any(sapply(random.variance, length) > 1)){
      stop("get_model_parameters doesn't handle random slopes yet.")
    }
    extra <- data.frame(
      Description = names(random.variance),
      Parent = parameter$Fingerprint[parameter$Description == 'Random effect variance'],
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
    parameter <- rbind(parameter, extra)
    tmp <- data.frame(
      Analysis = analysis@FileFingerprint,
      Parameter = extra$Fingerprint,
      Estimate = unlist(random.variance),
      LowerConfidenceLimit = NA,
      UpperConfidenceLimit = NA,
      stringsAsFactors = FALSE
    )
    rownames(tmp) <- NULL
    parameter.estimate <- rbind(parameter.estimate, tmp)
  
    # add random effect BLUP's
    random.effect <- ranef(analysis@Model, condVar = TRUE)
    extra <- data.frame(
      Description = names(random.effect),
      Parent = parameter$Fingerprint[parameter$Description == 'Random effect BLUP'],
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
    parameter <- rbind(parameter, extra)
    for(i in seq_along(random.effect)){
      extra.blup <- data.frame(
        Description = rownames(random.effect[[i]]),
        Parent = extra$Fingerprint[i],
        stringsAsFactors = FALSE
      )
      extra.blup$Fingerprint <- apply(extra.blup, 1, digest, algo = "sha1")
      parameter <- rbind(parameter, extra.blup)
      
      re <- random.effect[[i]][, 1]
      re.sd <- sqrt(attr(random.effect[[i]], "postVar")[1, 1, ])
      tmp <- data.frame(
        Analysis = analysis@FileFingerprint,
        Parameter = extra.blup$Fingerprint,
        Estimate = re,
        LowerConfidenceLimit = qnorm(0.025, mean = re, sd = re.sd),
        UpperConfidenceLimit = qnorm(0.975, mean = re, sd = re.sd),
        stringsAsFactors = FALSE
      )
      rownames(tmp) <- NULL
      parameter.estimate <- rbind(parameter.estimate, tmp)
    }  
    
    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)

#' @rdname get_model_parameter
#' @aliases get_model_parameter,n2kLrtGlmer-methods
#' @importFrom methods setMethod
#' @importFrom digest digest
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kLrtGlmer"),
  definition = function(analysis){
    if(analysis@Status != "converged"){
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "LRT test",
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, digest, algo = "sha1")
    
    parameter.estimate <- data.frame(
      Analysis = analysis@FileFingerprint,
      Parameter = parameter$Fingerprint,    
      Estimate = analysis@Anova[2, "Pr(>Chisq)"],
      LowerConfidenceLimit = NA,
      UpperConfidenceLimit = NA,
      stringsAsFactors = FALSE
    )
    row.names(parameter.estimate) <- NULL
    
    new("n2kParameter", Parameter = parameter, ParameterEstimate = parameter.estimate)
  }
)

#' @rdname get_model_parameter
#' @aliases get_model_parameter,n2kComposite-methods
#' @importFrom methods setMethod
#' @importFrom digest digest
#' @include n2kComposite_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kComposite"),
  definition = function(analysis){
    if(analysis@Status != "converged"){
      return(new("n2kParameter"))
    }
    
    parameter <- data.frame(
      Description = "Composite index",
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, digest, algo = "sha1")
    
    parameter.estimate <- cbind(
      Analysis = analysis@FileFingerprint,
      analysis@Index,
      stringsAsFactors = FALSE
    )
    colnames(parameter.estimate)[2] <- "Parameter"
    row.names(parameter.estimate) <- NULL
    parameter.estimate$Parameter <- levels(parameter.estimate$Parameter)[parameter.estimate$Parameter]
    
    extra <- data.frame(
      Description = parameter.estimate$Parameter,
      Parent = parameter$Fingerprint,
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
    parameter <- rbind(parameter, extra)
    
    parameter.estimate$Parameter <- extra$Fingerprint

    new("n2kParameter", Parameter = parameter, ParameterEstimate = parameter.estimate)
  }
)
