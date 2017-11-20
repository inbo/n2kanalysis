#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr select rename_ select_ inner_join arrange_ filter_ mutate_ bind_rows
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @include n2kInlaComparison_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaComparison"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # status: "new"
    if (status(x) == "new") {
      x@WAIC <- lapply(
          names(x@Models),
          function(parent){
            data.frame(
              Parent = parent,
              WAIC = x@Models[[parent]]$waic$waic,
              Peff = x@Models[[parent]]$waic$p.eff,
              stringsAsFactors = FALSE
            )
          }
        ) %>%
        bind_rows() %>%
        as.data.frame()
      status(x) <- "converged"
      return(x)
    }

    # status: "waiting"
    old.parent.status <- parent_status(x) %>%
      rename_(
        OldStatusFingerprint = ~ParentStatusFingerprint,
        OldStatus = ~ParentStatus
      )
    parents <- get_parents(child = x, base = dots$base, project = dots$project)
    compare <- lapply(
      parents,
      function(z){
        z@AnalysisMetadata %>%
          select(
            ParentAnalysis = .data$FileFingerprint,
            ParentStatusFingerprint = .data$StatusFingerprint,
            ParentStatus = .data$Status
          )
      }
    ) %>%
      bind_rows() %>%
      inner_join(old.parent.status, by = "ParentAnalysis") %>%
      arrange_(~ParentAnalysis)

    to.update <- compare %>% filter_(~ParentStatus == "converged")
    x@AnalysisRelation <- compare %>%
      select_(
        ~Analysis,
        ~ParentAnalysis,
        ~ParentStatusFingerprint,
        ~ParentStatus
      )

    if (any(x@AnalysisRelation$ParentStatus == "error")) {
      status(x) <- "error"
      return(x)
    }
    if (any(x@AnalysisRelation$ParentStatus == "false convergence")) {
      status(x) <- "false convergence"
      return(x)
    }
    if (nrow(to.update) == 0) {
      if (all(x@AnalysisRelation$ParentStatus == "unstable")) {
        status(x) <- "unstable"
      }
      return(x)
    }

    to.update <- to.update %>%
      mutate_(Filename = ~ paste0("/", status(x), "$") %>%
          gsub("/", dots$path) %>%
          paste0(ParentStatus, "/", ParentAnalysis, ".rds")
      ) %>%
      select_(~ParentAnalysis, ~Filename)
    models <- lapply(parents[to.update$ParentAnalysis], get_model)
    x@Models <- models

    if (all(
      x@AnalysisRelation$ParentStatus %in% c("converged", "unstable")
    )) {
      status(x) <- "new"
      return(fit_model(x, status = "new", ...))
    }
    status(x) <- "waiting"
    return(x)
  }
)
