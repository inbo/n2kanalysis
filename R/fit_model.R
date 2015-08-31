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
    standard.generic("fit_model") # nocov
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom n2khelper check_path read_object_environment
#' @importFrom assertthat assert_that is.flag
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
    if (is.null(dots$verbose)) {
      dots$verbose <- TRUE
    } else {
      assert_that(is.flag(dots$verbose))
    }
    if (dots$verbose) {
      message(x)
    }
    local.environment <- new.env()

    load(x, envir = local.environment)
    analysis <- read_object_environment(
      object = "analysis",
      env = local.environment
    )
    if (dots$verbose) {
      message(status(analysis), " -> ", appendLF = FALSE)
      utils::flush.console()
    }
    analysis.fitted <- fit_model(
      x = analysis,
      status = dots$status,
      path = dirname(x)
    )
    validObject(analysis.fitted)
    if (dots$verbose) {
      message(status(analysis.fitted))
      utils::flush.console()
    }
    assign("analysis", value = analysis.fitted, envir = local.environment)
    save(list = ls(local.environment), envir = local.environment, file = x)
    return(invisible(NULL))
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom lme4 glmer glmerControl
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kGlmerPoisson"),
  definition = function(x, ...){
    validObject(x)

    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    set.seed(get_seed(x))

    data <- get_data(x)
    model.formula <- x@AnalysisFormula[[1]]

    controls <- list(
      glmerControl(optimizer = "bobyqa"),
      glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
    )
    for (control in controls) {
      if ("optimx" %in% control$optimizer) {
        requireNamespace("optimx", quietly = TRUE)
      }

      if (grepl("^weighted", get_model_type(x))) {
        model <- try(glmer(
          formula = model.formula,
          data = data,
          family = poisson,
          weights = data$Weight,
          control = control
        ))
      } else {
        model <- try(glmer(
          formula = model.formula,
          data = data,
          family = poisson,
          control = control
        ))
      }
      if ("try-error" %in% class(model)) {
        next
      }
      if (length(model@optinfo$conv$lme4) == 0) {
        break
      }
    }
    if ("try-error" %in% class(model)) {
      status(x) <- "error"
      return(x)
    }
    if (length(model@optinfo$conv$lme4) > 0) {
      status(x) <- "false convergence"
      return(x)
    }
    vc <- VarCorr(model)
    if ("fObservation" %in% names(vc)) {
      olre.ratio <- exp(
        diff(
          qnorm(
            p = c(0.025, 0.975),
            mean = 0,
            sd = sqrt(vc[["fObservation"]])
          )
        )
      )
      if (olre.ratio > 1e4) {
        status(x) <- "unstable"
        return(x)
      }
    }
    x@Model <- model

    version <- get_analysis_version(sessionInfo())
    new.version <- union(x, version)
    x@AnalysisVersion <- new.version$Union@AnalysisVersion
    x@RPackage <- new.version$Union@RPackage
    x@AnalysisVersionRPackage <- new.version$Union@AnalysisVersionRPackage

    x@AnalysisMetadata$AnalysisVersion <- new.version$UnionFingerprint

    status(x) <- "converged"
    return(x)
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaNbinomial"),
  definition = function(x, ...){
    validObject(x)

    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    assert_that(requireNamespace("INLA", quietly = TRUE))

    set.seed(get_seed(x))

    data <- get_data(x)
    model.formula <- x@AnalysisFormula[[1]]

    link <- rep(NA, nrow(data))
    link[is.na(data$Count)] <- 1

    if (is.null(x@LinearCombination)) {
      lc <- NULL
    } else {
      lc <- x@LinearCombination
      tmp <- lapply(
        colnames(lc),
        function(i){
          lc[, i]
        }
      )
      names(tmp) <- colnames(lc)
      lc <- INLA::inla.make.lincombs(tmp)
      names(lc) <- rownames(x@LinearCombination)
    }
    inla.models <- INLA::inla.models
    model <- try(INLA::inla(
      formula = model.formula,
      family = "nbinomial",
      data = data,
      lincomb = lc,
      control.compute = list(dic = TRUE, cpo = TRUE),
      control.predictor = list(compute = TRUE, link = link),
      control.fixed = list(prec.intercept = 1)
    ))
    if ("try-error" %in% class(model)) {
      status(x) <- "error"
      return(x)
    }
    return(
      n2k_inla_nbinomial(data = x, model.fit = model, status = "converged")
    )
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }

    # stop if status doesn't require (re-)fitting the model
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # do calculation when all parents are available
    if (status(x) == "new") {
      #check for incorrect "new" status
      if (any(is.null(x@Model), is.null(x@Model0))) {
        x@AnalysisRelation$ParentStatusFingerprint <- "zzz"
        status(x) <- "waiting"
        return(fit_model(x, ...))
      }
      x@Anova <- anova(x@Model, x@Model0)
      status(x) <- "converged"
      return(x)
    }

    # check if parents are available
    if (is.null(dots$path)) {
      dots$path <- "."
    }
    old.parent.status <- parent_status(x)
    colnames(old.parent.status)[3:4] <- c("OldStatusFingerprint", "OldStatus")
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$ParentAnalysis, ".rda"),
      winslash = "/",
      mustWork = FALSE
    )
    if (!all(file_test("-f", files.to.check))) {
      status(x) <- "error"
      return(x)
    }

    #check if parents have changed
    current.parent.status <- status(files.to.check)[
      , c("FileFingerprint", "StatusFingerprint", "Status")
    ]
    colnames(current.parent.status)[1] <- "ParentAnalysis"
    compare <- merge(old.parent.status, current.parent.status)
    changes <- which(compare$OldStatusFingerprint != compare$StatusFingerprint)
    colnames(compare)[5:6] <- c("ParentStatusFingerprint", "ParentStatus")
    x@AnalysisRelation <- compare[
      order(compare$ParentAnalysis),
      c("Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
    ]
    if (any(current.parent.status == "error")) {
      status(x) <- "error"
      return(x)
    }
    if (any(current.parent.status == "false convergence")) {
      status(x) <- "false convergence"
      return(x)
    }
    if (any(current.parent.status == "unstable")) {
      status(x) <- "unstable"
      return(x)
    }
    if (length(changes) == 0) {
      if (all(current.parent.status$Status == "converged")) {
        status(x) <- "new"
      }
      return(x)
    }

    if (length(changes) == 2) {
      file <- paste0(dots$path, "/", x@Parent0, ".rda")
      x@Model0 <- get_model(file)
      parent.1 <- compare$ParentAnalysis[compare$ParentAnalysis != x@Parent0]
      file <- paste0(dots$path, "/", parent.1, ".rda")
      x@Model <- get_model(file)
    } else {
      file <- paste0(dots$path, "/", compare$ParentAnalysis[changes], ".rda")
      if (x@Parent0 == compare$ParentAnalysis[changes]) {
        x@Model0 <- get_model(file)
      } else {
        x@Model <- get_model(file)
      }
    }
    if (all(current.parent.status$Status == "converged")) {
      status(x) <- "new"
    } else {
      status(x) <- "waiting"
    }
    if (status(x) == "new") {
      return(fit_model(x, ...))
    }
    return(x)
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod
#' @include n2kComposite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kComposite"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }
    if (status(x) == "new") {
      parameter <- x@Parameter
      if (nrow(parameter) == 0) {
        status(x) <- "error"
        return(x)
      }
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
        p = 0.025,
        mean = index$Estimate,
        sd = sqrt(index$Variance)
      )
      index$UpperConfidenceLimit <- qnorm(
        p = 0.975,
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
    if (is.null(dots$path)) {
      dots$path <- "."
    }
    old.parent.status <- parent_status(x)
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$ParentAnalysis, ".rda"),
      winslash = "/",
      mustWork = FALSE
    )
    if (!all(file_test("-f", files.to.check))) {
      status(x) <- "error"
      return(x)
    }
    colnames(old.parent.status)[3:4] <- c("OldStatusFingerprint", "OldStatus")
    current.parent.status <- status(files.to.check)[
      , c("FileFingerprint", "StatusFingerprint", "Status")
    ]
    colnames(current.parent.status) <- c(
      "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus"
    )
    compare <- merge(old.parent.status, current.parent.status)
    converged <- which(compare$ParentStatus == "converged")
    x@AnalysisRelation <- compare[
      order(compare$ParentAnalysis),
      c("Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
    ]

    if (any(current.parent.status$ParentStatus == "error")) {
      status(x) <- "error"
      return(x)
    }
    if (any(current.parent.status$ParentStatus == "false convergence")) {
      status(x) <- "false convergence"
      return(x)
    }
    if (length(converged) == 0) {
      if (all(current.parent.status$ParentStatus == "unstable")) {
        status(x) <- "unstable"
      }
      return(x)
    }
    to.update <- compare$ParentAnalysis[converged]

    new.parameter <- do.call(rbind, lapply(to.update, function(parent){
      this.model <- get_model(paste0(dots$path, "/", parent, ".rda"))
      if (class(this.model) != "glmerMod") {
        stop("Composite with other class")
      }
      this.coef <- coef(summary(this.model))[, c("Estimate", "Std. Error")]
      this.coef <- as.data.frame(this.coef, stringsAsFactors = FALSE)
      this.coef$Variance <- this.coef$"Std. Error" ^ 2
      this.coef$"Std. Error" <- NULL
      this.coef$Parent <- parent
      this.coef$Value <- row.names(this.coef)
      rownames(this.coef) <- NULL
      covariate <- as.character(x@AnalysisFormula[[1]][2])
      this.coef <- this.coef[grep(covariate, this.coef$Value), ]
      this.coef$Value <- gsub(covariate, "", this.coef$Value)
      this.coef$Value <- factor(this.coef$Value, levels = this.coef$Value)
      return(this.coef)
    }))
    x@Parameter <- new.parameter[
      order(new.parameter$Parent, new.parameter$Value),
      c("Parent", "Value", "Estimate", "Variance")
    ]

    if (all(
      current.parent.status$ParentStatus %in% c("converged", "unstable")
    )) {
      status(x) <- "new"
      return(fit_model(x, ...))
    }
    status(x) <- "waiting"
    return(x)
  }
)
