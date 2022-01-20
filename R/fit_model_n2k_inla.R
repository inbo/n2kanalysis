#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.number
#' @importMethodsFrom multimput impute
#' @importFrom INLA inla inla.make.lincombs
#' @include n2k_inla_class.R
#' @param timeout the optional number of second until the model will time out
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInla"),
  definition = function(x, status = "new", ..., timeout = NULL) {
    validObject(x)
    assert_that(is.character(status), length(status) >= 1)

    # don't fit model when status doesn't match
    if (!(status(x) %in% status)) {
      return(x)
    }

    set.seed(get_seed(x))

    data <- get_data(x)
    model_formula <- x@AnalysisFormula[[1]]

    # prepare linear combinations
    if (is.null(x@LinearCombination)) {
      lc <- NULL
    } else {
      lincomb <- x@LinearCombination
      if (inherits(lincomb, "matrix")) {
        lc <- lincomb %>%
          as.data.frame() %>%
          as.list() %>%
          inla.make.lincombs()
        names(lc) <- rownames(lincomb)
      } else {
        lc <- inla.make.lincombs(lincomb)
        if (is.matrix(lincomb[[1]])) {
          names(lc) <- rownames(lincomb[[1]])
        } else {
          names(lc) <- names(lincomb[[1]])
        }
      }
    }
    # prepare inla() arguments
    control <- x@Control
    control$formula <- model_formula
    control$family <- x@Family
    control$data <- data
    control$lincomb <- lc
    # fit model
    model <- try({
        if (!is.null(timeout)) {
          assert_that(is.number(timeout), timeout > 0)
          setTimeLimit(cpu = timeout, elapsed = timeout)
        }
        do.call(inla, control)
        }, silent = TRUE)
    # handle error in model fit
    if (inherits(model, "try-error")) {
      status(x) <- "error"
      if (grepl("reached .* time limit", model)) {
        status(x) <- "time-out"
      }
      return(x)
    }
    # return fitted model when no imputation is required
    if (x@ImputationSize == 0) {
      return(n2k_inla(data = x, model_fit = model, status = "converged"))
    }

    # return fitted model with imputations
    return(n2k_inla(
      data = x, model_fit = model, status = "converged",
      raw_imputed = impute(
        model = model, n_imp = x@ImputationSize, minimum = x@Minimum
      )
    ))
  }
)
