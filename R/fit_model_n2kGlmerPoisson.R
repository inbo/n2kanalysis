#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom lme4 glmer glmerControl
#' @importFrom stats poisson qnorm
#' @importFrom utils sessionInfo
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
