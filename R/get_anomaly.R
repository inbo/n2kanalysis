#' Get the anomalies from a model
#' @param analysis The model to add
#' @param ... Extra options. See details.
#' @name get_anomaly
#' @rdname get_anomaly
#' @exportMethod get_anomaly
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_anomaly",
  def = function(analysis, ...) {
    standardGeneric("get_anomaly") # nocov
  }
)
