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
    }
    local.environment <- new.env()
    load(x, envir = local.environment)
    analysis <- read_object_environment(object = "analysis", env = local.environment)
    if(dots$verbose){
      message(status(analysis), " -> ", appendLF = FALSE)
      utils::flush.console()
    }
    analysis <- fit_model(
      x = analysis, 
      status = dots$status,
      path = dirname(x)
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
      dots$status <- c("new", "waiting")
    }
    if(!(status(x) %in% dots$status)){
      return(x)
    }
    
    set.seed(x@Seed)
    
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
    vc <- VarCorr(model)
    if("fRow" %in% names(vc)){
      olre.ratio <- exp(
        diff(
          qnorm(
            c(0.025, 0.975), 
            mean = 0, 
            sd = sqrt(vc[["fRow"]])
          )
        )
      )
      if(olre.ratio > 1e4){
        status(x) <- "unstable"
        return(x)
      }
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
      dots$status <- c("new", "waiting")
    }
    if(!(status(x) %in% dots$status)){
      return(x)
    }
    
    if(!requireNamespace("INLA", quietly = TRUE)){
      stop("The INLA package is required but not installed.")
    }
    
    set.seed(x@Seed)
    
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

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom digest digest
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, ...){
    dots <- list(...)
    if(is.null(dots$status)){
      dots$status <- c("new", "waiting")
    }
    
    # stop if status doesn't require (re-)fitting the model
    if(!(status(x) %in% dots$status)){
      return(x)
    }
    
    # do calculation when all parents are available
    if(status(x) == "new"){
      #check for incorrect "new" status
      if(any(is.null(x@Model), is.null(x@Model0))){
        x@ParentStatus$StatusFingerprint <- factor("zzz")
        status(x) <- "waiting"
        return(fit_model(x, ...))
      }
      x@Status <- "converged"
      x@Anova <- anova(x@Model, x@Model0)
      x@StatusFingerprint <- digest(
        list(
          x@FileFingerprint, x@Status, x@ParentStatus, x@Model, x@Model0, x@Anova, 
          x@SessionInfo
        ),
        algo = "sha1"
      )
      validObject(x)
      return(x)
    }
    
    # check if parents are available
    if(is.null(dots$path)){
      dots$path <- "."
    }
    old.parent.status <- x@ParentStatus
    colnames(old.parent.status)[2:3] <- c("OldStatusFingerprint", "OldStatus")
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$FileFingerprint, ".rda"),
      winslash = "/",
      mustWork = FALSE
    )
    if(!all(file_test("-f", files.to.check))){
      status(x) <- "error"
      return(x)
    }
    
    #check if parents have changed
    current.parent.status <- status(files.to.check)[, c("FileFingerprint", "StatusFingerprint", "Status")]
    if(any(current.parent.status == "error")){
      status(x) <- "error"
      return(x)
    }
    if(any(current.parent.status == "false convergence")){
      status(x) <- "false convergence"
      return(x)
    }
    if(any(current.parent.status == "unstable")){
      status(x) <- "unstable"
      return(x)
    }
    compare <- merge(old.parent.status, current.parent.status)
    compare$OldStatusFingerprint <- levels(compare$OldStatusFingerprint)[compare$OldStatusFingerprint]
    compare$StatusFingerprint <- levels(compare$StatusFingerprint)[compare$StatusFingerprint]
    changes <- which(compare$OldStatusFingerprint != compare$StatusFingerprint)
    if(length(changes) == 0){
      if(all(current.parent.status$Status == "converged")){
        status(x) <- "new"
      }
      return(x)
    }
    if(x@Parent %in% compare$FileFingerprint[changes]){
      file <- paste0(dots$path, "/", x@Parent, ".rda")
      x@Model <- get_model(file)
    }
    if(x@Parent0 %in% compare$FileFingerprint[changes]){
      file <- paste0(dots$path, "/", x@Parent, ".rda")
      x@Model0 <- get_model(file)
    }
    x@ParentStatus <- compare[
      order(compare$FileFingerprint), 
      c("FileFingerprint", "StatusFingerprint", "Status")
    ]
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, x@Status, x@ParentStatus, x@Model, x@Model0, x@Anova, 
        x@SessionInfo
      ),
      algo = "sha1"
    )
    if(all(current.parent.status$Status == "converged")){
      status(x) <- "new"
    }
    validObject(x)
    if(status(x) == "new"){
      return(fit_model(x, ...))
    }
    return(x)
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom digest digest
#' @include n2kComposite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kComposite"),
  definition = function(x, ...){
    dots <- list(...)
    if(is.null(dots$status)){
      dots$status <- c("new", "waiting")
    }
    if(!(status(x) %in% dots$status)){
      return(x)
    }
    if(status(x) == "new"){
      parameter <- x@Parameter
      # ignore parents which are missing in one of more years
      missing.parent <- unique(parameter$Parent[parameter$Estimate < -10])
      parameter <- parameter[!parameter$Parent %in% missing.parent, ]
      reference <- parameter[
        parameter$Value == levels(parameter$Value)[1], 
        c("Parent", "Estimate")
      ]
      colnames(reference)[2] <- "Reference"
      parameter <- merge(parameter, reference)
      parameter$Estimate <- parameter$Estimate - parameter$Reference
      index <- aggregate(
        cbind(parameter[, c("Estimate", "Variance")], N = 1),
        parameter[, "Value", drop = FALSE],
        FUN = sum
      )
      index$Estimate <- index$Estimate / index$N
      index$Variance <- index$Variance / (index$N ^ 2)
      index$LowerConfidenceLimit <- qnorm(
        0.025, 
        mean = index$Estimate, 
        sd = sqrt(index$Variance)
      )
      index$UpperConfidenceLimit <- qnorm(
        0.975, 
        mean = index$Estimate, 
        sd = sqrt(index$Variance)
      )
      x@Index <- index[
        order(index$Value), 
        c("Value", "Estimate", "LowerConfidenceLimit", "UpperConfidenceLimit")
      ]
      status(x) <- "converged"
      return(x)
    }
    if(is.null(dots$path)){
      dots$path <- "."
    }
    old.parent.status <- parent_status(x)
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$FileFingerprint, ".rda"),
      winslash = "/",
      mustWork = FALSE
    )
    if(!all(file_test("-f", files.to.check))){
      status(x) <- "error"
      return(x)
    }
    colnames(old.parent.status)[2:3] <- c("OldStatusFingerprint", "OldStatus")
    current.parent.status <- status(files.to.check)[, c("FileFingerprint", "StatusFingerprint", "Status")]
    if(any(current.parent.status == "error")){
      status(x) <- "error"
      return(x)
    }
    if(any(current.parent.status == "false convergence")){
      status(x) <- "false convergence"
      return(x)
    }
    if(all(current.parent.status$Status %in% c("converged", "unstable"))){
      status(x) <- "new"
    }
    compare <- merge(old.parent.status, current.parent.status)
    compare$OldStatusFingerprint <- levels(compare$OldStatusFingerprint)[compare$OldStatusFingerprint]
    compare$StatusFingerprint <- levels(compare$StatusFingerprint)[compare$StatusFingerprint]
    changes <- which(compare$Status == "converged")
    if(length(changes) == 0){
      return(x)
    }
    to.update <- x@Parent[x@Parent %in% compare$FileFingerprint[changes]]
    
    new.parameter <- do.call(rbind, lapply(to.update, function(parent){
      this.model <- get_model(paste0(dots$path, "/", parent, ".rda"))
      if(class(this.model) == "glmerMod"){
        this.coef <- coef(summary(this.model))[, c("Estimate", "Std. Error")]
        this.coef <- as.data.frame(this.coef, stringsAsFactors = FALSE)
        this.coef$Variance <- this.coef$"Std. Error" ^ 2
        this.coef$"Std. Error" <- NULL
      } else {
        stop("Composite with other class")
      }
      this.coef$Parent <- parent
      this.coef$Value <- row.names(this.coef)
      rownames(this.coef) <- NULL
      this.coef <- this.coef[grep(x@Covariate, this.coef$Value), ]
      this.coef$Value <- gsub(x@Covariate, "", this.coef$Value)
      this.coef$Value <- factor(this.coef$Value, levels = this.coef$Value)
      new.parameter <- rbind(x@Parameter, this.coef)
      x@Parameter <- new.parameter[
        order(new.parameter$Parent, new.parameter$Value), 
        c("Parent", "Value", "Estimate", "Variance")
      ]
    }
    x@ParentStatus <- compare[
      order(compare$FileFingerprint), 
      c("FileFingerprint", "StatusFingerprint", "Status")
    ]
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, x@Status, x@ParentStatus, x@Parameter, x@Index, x@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(x)
    if(status(x) == "new"){
      return(fit_model(x, ...))
    }
    return(x)
  }
)
