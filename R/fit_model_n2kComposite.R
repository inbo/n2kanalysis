#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% filter group_by n summarise transmute mutate arrange
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @importFrom stats qnorm
#' @include n2kComposite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kComposite"),
  definition = function(x, base, project, status = "new", ...) {
    validObject(x)
    assert_that(
      is.character(status),
      length(status) >= 1
    )
    if (!(status(x) %in% status)) {
      return(x)
    }

    if (status(x) == "new") {
      parameter <- x@Parameter
      if (nrow(parameter) == 0) {
        status(x) <- "error"
        return(x)
      }
      x@Parameter %>%
        filter(!is.na(.data$Estimate), !is.na(.data$Variance)) %>%
        group_by(.data$Value) %>%
        summarise(
          Estimate = mean(.data$Estimate),
          SE = sqrt(sum(.data$Variance)) / n()
        ) %>%
        transmute(
          .data$Value,
          .data$Estimate,
          LowerConfidenceLimit =
            qnorm(0.025, mean = .data$Estimate, sd = .data$SE),
          UpperConfidenceLimit =
            qnorm(0.975, mean = .data$Estimate, sd = .data$SE)
        ) %>%
        as.data.frame() -> x@Index
      status(x) <- "converged"
      return(x)
    }

    status(x) <- "waiting"
    parent.status <- parent_status(x)
    parent.status %>%
      filter(.data$ParentStatus %in% c("new", "waiting", status)) %>%
      pull("ParentAnalysis") -> todo
    if (length(todo) == 0) {
      return(x)
    }

    for (parent in todo) {
      model <- read_model(x = parent, base = base, project = project)
      if (status(model) %in% c("new", "waiting")) {
        return(x)
      }
      parent.status[parent.status$ParentAnalysis == parent, "ParentStatus"] <-
        status(model)
      parent.status[
        parent.status$ParentAnalysis == parent,
        "ParentStatusFingerprint"
      ] <- get_status_fingerprint(model)
      x@AnalysisRelation <- parent.status
      if (status(model) == "converged") {
        extract(
          extractor = x@Extractor,
          object = model
        ) %>%
          mutate(Parent = parent) %>%
          bind_rows(
            x@Parameter %>%
              filter(.data$Parent != parent)
          ) %>%
          arrange(.data$Parent, .data$Value) -> x@Parameter
        if (all(parent.status$ParentStatus == "converged")) {
          status(x) <- "new"
        } else {
          status(x) <- status(x)
        }
      } else {
        status(x) <- "error"
        return(x)
      }
    }

    if (status(x) != "new") {
      return(x)
    }
    fit_model(x, status = "new", base = base, project = project, ...)
  }
)
