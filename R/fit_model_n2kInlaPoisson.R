#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that
#' @importMethodsFrom multimput impute
#' @importFrom INLA inla
#' @include n2kInlaPoisson_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaPoisson"),
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

    response <- data[, as.character(x@AnalysisFormula[[1]][[2]])]
    link <- ifelse(is.na(response), 1, NA)

    if (is.null(x@LinearCombination)) {
      lc <- NULL
    } else {
      lincomb <- x@LinearCombination
      if (class(lincomb) == "matrix") {
        lc <- lincomb %>%
          as.data.frame() %>%
          as.list() %>%
          INLA::inla.make.lincombs()
        names(lc) <- rownames(lincomb)
      } else {
        lc <- INLA::inla.make.lincombs(lincomb)
        if (is.matrix(lincomb[[1]])) {
          names(lc) <- rownames(lincomb[[1]])
        } else {
          names(lc) <- names(lincomb[[1]])
        }
      }
    }
    model <- try({
      if (!is.null(dots$timeout)) {
        setTimeLimit(cpu = dots$timeout, elapsed = dots$timeout)
      }
      inla(
        formula = model.formula,
        family = "poisson",
        data = data,
        lincomb = lc,
        control.compute = list(
          dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE
        ),
        control.predictor = list(compute = TRUE, link = link),
        control.fixed = list(prec.intercept = 1)
      )
    })
    if (inherits(model, "try-error")) {
      if (grepl("reached .* time limit", model)) {
        status(x) <- "time-out"
      } else {
        status(x) <- "error"
      }
      return(x)
    }
    if (x@ImputationSize == 0) {
      return(
        n2k_inla_poisson(data = x, model.fit = model, status = "converged")
      )
    }
    return(
      n2k_inla_poisson(
        data = x,
        model.fit = model,
        raw.imputed = impute(
          model = model,
          n.imp = x@ImputationSize,
          minimum = x@Minimum
        ),
        status = "converged"
      )
    )
  }
)
