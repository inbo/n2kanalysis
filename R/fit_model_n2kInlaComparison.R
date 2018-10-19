#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter pull bind_rows
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @include n2kInlaComparison_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaComparison"),
  definition = function(x, base, project, status = "new", verbose = TRUE, ...){
    validObject(x)

    assert_that(
      is.character(status),
      length(status) >= 1
    )
    if (!(status(x) %in% status)) {
      return(x)
    }

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
        next
      }
      parent.status[parent.status$ParentAnalysis == parent, "ParentStatus"] <-
        status(model)
      parent.status[
        parent.status$ParentAnalysis == parent,
        "ParentStatusFingerprint"
      ] <- get_status_fingerprint(model)
      x@AnalysisRelation <- parent.status
      if (status(model) == "converged") {
        update_waic <- data.frame(
          Parent = parent,
          WAIC = model@Model$waic$waic,
          Peff = model@Model$waic$p.eff,
          stringsAsFactors = FALSE
        )
        if (is.null(x@WAIC)) {
          x@WAIC <- update_waic
        } else {
          x@WAIC %>%
            filter(.data$Parent != parent) %>%
            bind_rows(update_waic) -> x@WAIC
        }
        if (all(parent.status$ParentStatus == "converged")) {
          status(x) <- "converged"
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
