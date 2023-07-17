#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.number
#' @importFrom multimput hurdle_impute
#' @inheritParams multimput::hurdle_impute
#' @include n2k_hurdle_imputed_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kHurdleImputed"),
  definition = function(
    x, base, project, status = c("new", "waiting"), ...
) {
    validObject(x)
    assert_that(is.character(status), length(status) >= 1)

    # don't fit model when status doesn't match
    if (!(status(x) %in% status)) {
      return(x)
    }

    if (status(x) != "new") {
      parents <- get_parents(x, base = base, project = project)
      presence <- parents[[x@AnalysisRelation$parent_analysis[1]]]
      count <- parents[[x@AnalysisRelation$parent_analysis[2]]]
      x@Presence <- presence@RawImputed
      x@Count <- count@RawImputed
      x@AnalysisRelation$parentstatus_fingerprint[1] <- get_status_fingerprint(
        presence
      )
      x@AnalysisRelation$parentstatus_fingerprint[2] <- get_status_fingerprint(
        count
      )
      x@AnalysisRelation$parentstatus[1] <- status(presence)
      x@AnalysisRelation$parentstatus[2] <- status(count)
      x@AnalysisMetadata$status <- ifelse(
        all(x@AnalysisRelation$parentstatus == "converged"), "new",
        ifelse(
          any(!x@AnalysisRelation$parentstatus %in%
                c("new", "waiting", "converged")),
          "error", "waiting"
        )
      )
      x@AnalysisMetadata$status_fingerprint <- sha1(
        list(
          get_file_fingerprint(x), x@AnalysisMetadata$status,
          x@AnalysisVersion$fingerprint, x@AnalysisVersion, x@RPackage,
          x@AnalysisVersionRPackage, x@AnalysisRelation, x@Presence, x@Count,
          x@Hurdle
        ),
        digits = 6L
      )
    }

    if (status(x) != "new") {
      return(x)
    }

    result <- try(hurdle_impute(x@Presence, x@Count))
    if (inherits(result, "try-error")) {
      x@AnalysisMetadata$status_fingerprint <- sha1(
        list(
          get_file_fingerprint(x), "error", x@AnalysisVersion$fingerprint,
          x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
          x@AnalysisRelation, x@Presence, x@Count, NULL
        ),
        digits = 6L
      )
      x@AnalysisMetadata$status <- "error"
      return(x)
    }
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        get_file_fingerprint(x), "converged", x@AnalysisVersion$fingerprint,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation, x@Presence, x@Count, result
      ),
      digits = 6L
    )
    x@AnalysisMetadata$status <- "converged"
    x@Hurdle <- result
    return(x)
  }
)
