#' @name result_metadata
#' @rdname result_metadata
#' @exportMethod result_metadata
#' @docType methods
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
