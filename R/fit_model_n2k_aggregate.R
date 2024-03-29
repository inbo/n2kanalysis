#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom multimput aggregate_impute
#' @importFrom stats setNames
#' @include n2k_aggregate_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kAggregate"),
  definition = function(x, ...) {
    validObject(x)
    dots <- list(...)
    dots$status <- coalesce(list(dots$status), list(c("new", "waiting")))[[1]]
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # status: "waiting"
    if (status(x) != "new" | is.null(x@RawImputed)) {
      parent <- get_parents(x, base = dots$base, project = dots$project)
      if (length(parent) != 1) {
        status(x) <- "error"
        return(x)
      }
      parent <- parent[[1]]
      parent_status <- status(parent)
      if (parent_status %in% c("new", "waiting")) {
        status(x) <- "waiting"
        return(x)
      }
      if (parent_status != "converged") {
        status(x) <- "error"
        return(x)
      }
      if (inherits(parent, "n2kInla")) {
        x@RawImputed <- parent@RawImputed
      } else if (inherits(parent, "n2kAggregate")) {
        nrow(parent@AggregatedImputed@Covariate) |>
          rep(x = NA_integer_) |>
          list() |>
          setNames("Count") |>
          cbind(parent@AggregatedImputed@Covariate) |>
          list() |>
          setNames("Data") |>
          c(
            list(
              Class = "rawImputed", Response = "Count", Minimum = "",
              Imputation = parent@AggregatedImputed@Imputation,
              Extra = cbind(
                parent@AggregatedImputed@Covariate[0, ], Count = integer(0)
              )
            )
          ) |>
          do.call(what = "new") -> x@RawImputed
      } else if (inherits(parent, "n2kHurdleImputed")) {
        x@RawImputed <- parent@Hurdle
      } else {
        stop("cannot handle a parent of class ", class(parent))
      }
      x@AnalysisRelation$parent_status <- parent@AnalysisMetadata$status
      x@AnalysisRelation$parentstatus_fingerprint <-
        parent@AnalysisMetadata$status_fingerprint
      status(x) <- "new"
    }
    model <- try(
      aggregate_impute(
        object = x@RawImputed,
        grouping = x@AnalysisFormula[[1]] %>%
          terms() %>%
          attr("term.labels"),
        fun = x@Function,
        filter = x@Filter,
        join = x@Join
      )
    )
    if ("try-error" %in% class(model)) {
      status(x) <- "error"
      return(x)
    }
    x@AggregatedImputed <- model
    status(x) <- "converged"
    return(x)
  }
)
