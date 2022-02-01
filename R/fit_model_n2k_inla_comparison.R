#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter pull bind_rows
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @include n2k_inla_comparison_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaComparison"),
  definition = function(x, base, project, status = "new", verbose = TRUE, ...) {
    validObject(x)

    assert_that(
      is.character(status),
      length(status) >= 1
    )
    if (!(status(x) %in% status)) {
      return(x)
    }

    status(x) <- "waiting"
    parent_status <- parent_status(x)
    parent_status %>%
      filter(.data$parent_status %in% c("new", "waiting", status)) %>%
      pull("parent_analysis") -> todo
    if (length(todo) == 0) {
      return(x)
    }

    for (parent in todo) {
      model <- read_model(x = parent, base = base, project = project)
      if (status(model) %in% c("new", "waiting")) {
        next
      }
      parent_status[parent_status$parent_analysis == parent, "parent_status"] <-
        status(model)
      parent_status[
        parent_status$parent_analysis == parent,
        "parentstatus_fingerprint"
      ] <- get_status_fingerprint(model)
      x@AnalysisRelation <- parent_status
      if (status(model) == "converged") {
        update_waic <- data.frame(
          parent = parent,
          waic = model@Model$waic$waic,
          p_eff = model@Model$waic$p.eff,
          stringsAsFactors = FALSE
        )
        if (is.null(x@WAIC)) {
          x@WAIC <- update_waic
        } else {
          x@WAIC %>%
            filter(.data$parent != parent) %>%
            bind_rows(update_waic) -> x@WAIC
        }
        if (all(parent_status$parent_status == "converged")) {
          status(x) <- "converged"
        } else {
          status(x) <- status(x)
        }
      } else {
        status(x) <- "error"
      }
      if (status(x) == "error") {
        return(x)
      }
    }

    return(x)
  }
)
