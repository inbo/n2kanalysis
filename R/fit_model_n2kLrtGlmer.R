#' @rdname fit_model
#' @importFrom methods setMethod new slot
#' @importFrom utils file_test
#' @importFrom stats anova
#' @importFrom dplyr %>% arrange transmute
#' @importFrom rlang .data
#' @importFrom purrr map_df
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, ...) {
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
    if (status(x) == "new" && !is.null(x@Model) && !is.null(x@Model0)) {
      x@Anova <- anova(x@Model, x@Model0)
      status(x) <- "converged"
      return(x)
    }

    parents <- get_parents(child = x, base = dots$base, project = dots$project)
    parents %>%
      map_df(slot, name = "AnalysisMetadata") %>%
      transmute(
        Analysis = get_file_fingerprint(x),
        ParentAnalysis = .data$FileFingerprint,
        ParentStatusFingerprint = .data$StatusFingerprint,
        ParentStatus = .data$Status
      ) %>%
      arrange(.data$ParentAnalysis) -> x@AnalysisRelation
    parents[[x@Parent0]] %>%
      get_model() -> x@Model0
    parents[names(parents) != x@Parent0] %>%
      `[[`(1) %>%
      get_model() -> x@Model
    if (any(x@AnalysisRelation$ParentStatus == "error")) {
      status(x) <- "error"
    } else if (any(x@AnalysisRelation$ParentStatus == "false_convergence")) {
      status(x) <- "false_convergence"
    } else if (any(x@AnalysisRelation$ParentStatus == "unstable")) {
      status(x) <- "unstable"
    } else if (all(x@AnalysisRelation$ParentStatus == "converged"))  {
      status(x) <- "new"
    } else {
      status(x) <- "waiting"
    }

    return(fit_model(x, status = "new", ...))
  }
)
