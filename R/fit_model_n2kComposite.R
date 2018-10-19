#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% select select_ group_by_ summarise_ distinct_ filter_ anti_join arrange_ inner_join rename
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @importFrom stats qnorm
#' @include n2kComposite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kComposite"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }
    if (status(x) == "new") {
      parameter <- x@Parameter
      if (nrow(parameter) == 0) {
        status(x) <- "error"
        return(x)
      }
      # ignore parents which are missing in one of more years
      missing.parent <- parameter %>%
        filter_(~Estimate < -10) %>%
        select_(~Parent) %>%
        distinct_()
      x@Index <- anti_join(parameter, missing.parent, by = "Parent") %>%
        group_by_(~Value) %>%
        summarise_(
          Estimate = ~mean(Estimate),
          SE = ~ sqrt(sum(Variance) / n()),
          LowerConfidenceLimit = ~qnorm(0.025, mean = Estimate, sd = SE),
          UpperConfidenceLimit = ~qnorm(0.975, mean = Estimate, sd = SE)
        ) %>%
        select_(~-SE) %>%
        as.data.frame()
      status(x) <- "converged"
      return(x)
    }

    # status: "waiting"
    old.parent.status <- parent_status(x) %>%
      rename(
        OldStatusFingerprint = "ParentStatusFingerprint",
        OldStatus = "ParentStatus"
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

    to.update <- compare %>%
      filter_(~ParentStatus == "converged")
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

    if (nrow(to.update) > 0) {
      x@Parameter <- extract(
        extractor = x@Extractor,
        object = to.update$ParentAnalysis,
        base = dots$base,
        project = dots$project
      ) %>%
        arrange_(~Parent, ~Value)
    }

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
