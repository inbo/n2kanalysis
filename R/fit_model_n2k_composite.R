#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr arrange filter group_by mutate n summarise transmute
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @importFrom stats qnorm
#' @include n2k_composite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kComposite"),
  definition = function(x, base, project, status = "new", ...) {
    validObject(x)
    assert_that(is.character(status), length(status) >= 1)
    if (!(status(x) %in% status)) {
      return(x)
    }

    if (status(x) == "new") {
      parameter <- x@Parameter
      if (nrow(parameter) == 0) {
        status(x) <- "error"
        return(x)
      }
      x@Parameter |>
        filter(!is.na(.data$estimate), !is.na(.data$variance)) |>
        group_by(.data$value) |>
        summarise(
          estimate = mean(.data$estimate), se = sqrt(sum(.data$variance)) / n()
        ) |>
        transmute(
          .data$value, .data$estimate,
          lower_confidence_limit =
            qnorm(0.025, mean = .data$estimate, sd = .data$se),
          upper_confidence_limit =
            qnorm(0.975, mean = .data$estimate, sd = .data$se)
        ) |>
        as.data.frame() -> x@Index
      status(x) <- "converged"
      return(x)
    }

    status(x) <- "waiting"
    parent_status <- parent_status(x)
    parent_status |>
      filter(.data$parent_status %in% c("new", "waiting", status)) |>
      pull("parent_analysis") -> todo

    for (this_parent in todo) {
      model <- try(
        read_model(x = this_parent, base = base, project = project),
        silent = TRUE
      )
      if (inherits(model, "try-error")) {
        parent_status[
          parent_status$parent_analysis == this_parent, "parent_status"
        ] <- "error"
        next
      }
      parent_status[
        parent_status$parent_analysis == this_parent, "parent_status"
      ] <- status(model)
      parent_status[
        parent_status$parent_analysis == this_parent,
        "parentstatus_fingerprint"
      ] <- get_status_fingerprint(model)
      x@AnalysisRelation <- parent_status
      if (status(model) == "converged") {
        extract(extractor = x@Extractor, object = model) |>
          mutate(parent = this_parent) |>
          bind_rows(
            x@Parameter |>
              filter(.data$parent != this_parent)
          ) |>
          arrange(.data$parent, .data$value) -> x@Parameter
      }
    }
    if (all(parent_status$parent_status == "converged")) {
      status(x) <- "new"
    } else if (any(parent_status$parent_status == "error")) {
      status(x) <- "error"
    } else {
      status(x) <- "waiting"
    }

    fit_model(x, status = "new", base = base, project = project, ...)
  }
)
