#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom utils file_test
#' @importFrom stats anova
#' @importFrom dplyr %>% select
#' @importFrom rlang .data
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }

    # stop if status doesn't require (re-)fitting the model
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # do calculation when all parents are available
    if (status(x) == "new") {
      #check for incorrect "new" status
      if (any(is.null(x@Model), is.null(x@Model0))) {
        x@AnalysisRelation$ParentStatusFingerprint <- "zzz"
        status(x) <- "waiting"
        return(fit_model(x, status = "waiting", ...))
      }
      x@Anova <- anova(x@Model, x@Model0)
      status(x) <- "converged"
      return(x)
    }

    # check if parents are available
    old.parent.status <- parent_status(x)
    colnames(old.parent.status)[3:4] <- c("OldStatusFingerprint", "OldStatus")
    parents <- get_parents(child = x, base = dots$base, project = dots$project)
    current.parent.status <- lapply(
      parents,
      function(z){
        z@AnalysisMetadata %>%
          select(
            ParentAnalysis = .data$FileFingerprint,
            .data$StatusFingerprint,
            .data$Status
          )
      }
    ) %>%
      bind_rows()

    #check if parents have changed
    compare <- merge(old.parent.status, current.parent.status)
    changes <- which(compare$OldStatusFingerprint != compare$StatusFingerprint)
    colnames(compare)[5:6] <- c("ParentStatusFingerprint", "ParentStatus")
    x@AnalysisRelation <- compare[
      order(compare$ParentAnalysis),
      c("Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
    ]
    if (any(current.parent.status == "error")) {
      status(x) <- "error"
      return(x)
    }
    if (any(current.parent.status == "false convergence")) {
      status(x) <- "false convergence"
      return(x)
    }
    if (any(current.parent.status == "unstable")) {
      status(x) <- "unstable"
      return(x)
    }
    if (length(changes) == 0) {
      if (all(current.parent.status$Status == "converged")) {
        status(x) <- "new"
      }
      return(x)
    }

    if (length(changes) == 2) {
      file <- paste0(dots$path, "/", x@Parent0, ".rds")
      x@Model0 <- get_model(file)
      parent.1 <- compare$ParentAnalysis[compare$ParentAnalysis != x@Parent0]
      file <- paste0(dots$path, "/", parent.1, ".rds")
      x@Model <- get_model(file)
    } else {
      file <- paste0(dots$path, "/", compare$ParentAnalysis[changes], ".rds")
      if (x@Parent0 == compare$ParentAnalysis[changes]) {
        x@Model0 <- get_model(file)
      } else {
        x@Model <- get_model(file)
      }
    }
    if (all(current.parent.status$Status == "converged")) {
      status(x) <- "new"
    } else {
      status(x) <- "waiting"
    }
    if (status(x) == "new") {
      return(fit_model(x, status = "new", ...))
    }
    return(x)
  }
)
