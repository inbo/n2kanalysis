#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr rename_ select_ inner_join arrange_ filter_ mutate_ bind_rows
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
    if (is.null(dots$path)) {
      dots$path <- "."
    }
    if (inherits(dots$path, "s3_object")) {
      stop("path to S3 object not handled yet")
    }
    old.parent.status <- parent_status(x)
    available <- paste0("/", status(x), "$") %>%
      gsub("", dots$path) %>%
      list.files(
        pattern = "\\.rds$",
        recursive = TRUE,
        full.names = TRUE
      )
    if (
      !all(
        old.parent.status$ParentAnalysis %in%
          gsub(".*([0-9a-f]{40})\\.rds", "\\1", available)
      )
    ) {
      status(x) <- "error"
      return(x)
    }
    old.parent.status <- old.parent.status %>%
      rename_(
        OldStatusFingerprint = ~ParentStatusFingerprint,
        OldStatus = ~ParentStatus
      )
    compare <- status(available) %>%
      select_(
        ParentAnalysis = ~FileFingerprint,
        ParentStatusFingerprint = ~StatusFingerprint,
        ParentStatus = ~Status
      ) %>%
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
    models <- lapply(to.update$Filename, get_model)
    names(models) <- to.update$ParentAnalysis
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
