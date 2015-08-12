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
  contains = "n2kParameter",
  prototype = prototype(
    AnomalyType = data.frame(
      Description = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    ),
    Anomaly = data.frame(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      DatasourceID = integer(0),
      Datafield = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kAnomaly",
  function(object){
    required.class <- list(
      Description = c("character", "factor"),
      Fingerprint = c("character", "factor", "integer")
    )
    check_dataframe_variable(
      df = object@AnomalyType,
      variable = required.class,
      name = "AnomalyType"
    )

    required.class <- list(
      AnomalyType = c("character", "factor", "integer"),
      Analysis = c("character", "factor"),
      Parameter = c("character", "factor", "integer"),
      DatasourceID = "integer",
      Datafield = c("character", "factor")
    )
    check_dataframe_variable(
      df = object@Anomaly,
      variable = required.class,
      name = "Anomaly"
    )

    if (!all(object@Anomaly$AnomalyType %in% object@AnomalyType$Fingerprint)) {
      stop("Some Anomaly have no matching Fingerprint in 'AnomalyType'")
    }

    current <- object@Anomaly[, c("Analysis", "Parameter")]
    merged <- merge(
      current,
      object@ParameterEstimate[, c("Analysis", "Parameter")]
    )
    if (nrow(current) != nrow(merged)) {
      stop("Mismatch on Parameter between Anomaly and ParameterEstimate slot")
    }

    return(TRUE)
  }
)
