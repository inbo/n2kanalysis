#' @rdname result_metadata
#' @importFrom dplyr left_join rename select
#' @importFrom methods setMethod
setMethod(
  f = "result_metadata",
  signature = signature(x = "n2kResult"),
  definition = function(x, ...) {
    validObject(x)
    slot(x, "AnalysisMetadata") |>
      rename(analysis = "file_fingerprint") |>
      left_join(
        slot(x, "AnalysisRelation") |>
          select("analysis", "parent_analysis"),
        by = "analysis"
      )
  }
)
