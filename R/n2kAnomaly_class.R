#' The n2kAnomaly class
#' @name n2kAnomaly-class
#' @rdname n2kAnomaly-class
#' @exportClass n2kAnomaly
#' @aliases n2kAnomaly-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kAnomaly",
  representation = representation(
    AnomalyType = "data.frame",
    Anomaly = "data.frame"
  ),
  prototype = prototype(
    AnomalyType = data.frame(
      Description = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    ),
    Anomaly = data.frame(
      Analysis = character(0),
      AnomalyType = character(0),
      ObservationID = character(0),
      Estimate = numeric(0),
      stringsAsFactors = FALSE
    )
  )
)
