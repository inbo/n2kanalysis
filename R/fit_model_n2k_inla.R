#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.number
#' @importMethodsFrom multimput impute
#' @include n2k_inla_class.R
#' @param timeout the optional number of second until the model will time out
#' @inheritParams multimput::impute
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInla"),
  definition = function(
    x, status = "new", ..., timeout = NULL, seed = get_seed(x),
    num_threads = NULL, parallel_configs = TRUE
  ) {
    assert_that(
      requireNamespace("INLA", quietly = TRUE),
      msg = "INLA package required but not installed."
    )
    validObject(x)
    assert_that(is.character(status), length(status) >= 1)

    # don't fit model when status doesn't match
    if (!(status(x) %in% status)) {
      return(x)
    }

    set.seed(seed)

    data <- get_data(x)
    model_formula <- x@AnalysisFormula[[1]]

    # prepare linear combinations

    lc <- model2lincomb(x@LinearCombination)
    # prepare inla() arguments
    control <- x@Control
    control$formula <- model_formula
    control$family <- x@Family
    # fit model
    fm <- terms(x@AnalysisFormula[[1]])
    response <- all.vars(fm)[attr(fm, "response")]
    if (mean(is.na(data[[response]])) < 0.10) {
      # directly fit model when less than 10% missing data
      control$data <- data
      control$lincomb <- lc
      model <- try({
        if (!is.null(timeout)) {
          assert_that(is.number(timeout), timeout > 0)
          setTimeLimit(cpu = timeout, elapsed = timeout)
        }
        do.call(INLA::inla, control)
      }, silent = TRUE)
    } else {
      # first fit model without missing data
      control$data <- data[!is.na(data[[response]]), ]
      m0 <- try({
        if (!is.null(timeout)) {
          assert_that(is.number(timeout), timeout > 0)
          setTimeLimit(cpu = timeout, elapsed = timeout)
        }
        do.call(INLA::inla, control)
      }, silent = TRUE)
      if (inherits(m0, "try-error") && "control.family" %in% names(control)) {
        control$control.family <- NULL
        m0 <- try({
          if (!is.null(timeout)) {
            assert_that(is.number(timeout), timeout > 0)
            setTimeLimit(cpu = timeout, elapsed = timeout)
          }
          do.call(INLA::inla, control)
        }, silent = TRUE)
        control$control.family <- x@Control$control.family
      }
      if (inherits(m0, "try-error")) {
        status(x) <- ifelse(
          grepl("time limit", m0), "time-out", "error"
        )
        return(x)
      }
      # then refit with missing data
      control$data <- data
      control$lincomb <- lc
      control$control.update <- list(result = m0)
      model <- try({
        if (!is.null(timeout)) {
          assert_that(is.number(timeout), timeout > 0)
          setTimeLimit(cpu = timeout, elapsed = timeout)
        }
        do.call(INLA::inla, control)
      }, silent = TRUE)
    }

    # handle error in model fit
    if (inherits(model, "try-error")) {
      status(x) <- ifelse(
        grepl("time limit", model), "time-out", "error"
      )
      return(x)
    }
    # return fitted model when no imputation is required
    if (x@ImputationSize == 0) {
      return(n2k_inla(data = x, model_fit = model, status = "converged"))
    }

    imputed <- try(impute(
      model = model, n_imp = x@ImputationSize, minimum = x@Minimum,
      seed = seed, num_threads = num_threads, extra = x@Extra,
      parallel_configs = parallel_configs
    ))
    if (inherits(imputed, "try-error")) {
      return(n2k_inla(data = x, model_fit = model, status = "error"))
    }
    # return fitted model with imputations
    return(n2k_inla(
      data = x, model_fit = model, status = "converged", raw_imputed = imputed
    ))
  }
)

model2lincomb <- function(lincomb) {
  if (is.null(lincomb)) {
    return(NULL)
  }
  if (inherits(lincomb, "matrix")) {
    lincomb |>
      as.data.frame() |>
      as.list() %>%
      INLA::inla.make.lincombs() |>
      setNames(rownames(lincomb)) -> lc
    return(lc)
  }
  lc <- INLA::inla.make.lincombs(lincomb)
  if (is.matrix(lincomb[[1]])) {
    names(lc) <- rownames(lincomb[[1]])
  } else {
    names(lc) <- names(lincomb[[1]])
  }
  return(lc)
}
