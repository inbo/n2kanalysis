#' The n2kParameter class
#' @name n2kParameter-class
#' @rdname n2kParameter-class
#' @exportClass n2kParameter
#' @aliases n2kParameter-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kParameter",
  representation = representation(
    Parameter = "data.frame",
    ParameterEstimate = "data.frame"
  ),
  prototype = prototype(
    Parameter = data.frame(
      Description = character(0),
      Parent = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    ),
    ParameterEstimate = data.frame(
      Analysis = character(0),
      Parameter = character(0),
      Estimate = numeric(0),
      LowerConfidenceLimit = numeric(0),
      UpperConfidenceLimit = numeric(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kParameter",
  function(object){
    check_dataframe_variable(
      df = object@Parameter,
      variable = c("Description", "Parent", "Fingerprint"),
      name = "Parameter"
    )
    check_dataframe_variable(
      df = object@ParameterEstimate,
      variable = c(
        "Analysis", "Parameter", "Estimate", "LowerConfidenceLimit",
        "UpperConfidenceLimit"
      ),
      name = "ParameterEstimate"
    )
    if (!all(
      na.omit(object@Parameter$Parent) %in% object@Parameter$Fingerprint
    )) {
      stop("Some Parent in 'Parameter' slot not found")
    }
    if (!all(
      object@ParameterEstimate$Parameter %in% object@Parameter$Fingerprint
    )) {
      stop(
"Some Parameter in 'ParameterEstimate' slot have no matching Fingerprint in
'Parameter' slot"
      )
    }
    if (anyDuplicated(object@Parameter$Fingerprint)) {
      stop("Duplicated Fingerprint in 'Parameter' slot")
    }
    if (anyDuplicated(object@Parameter[, c("Description", "Parent")])) {
      stop("Duplicated rows in 'Parameter' slot")
    }
    if (anyDuplicated(object@ParameterEstimate[, c("Analysis", "Parameter")])) {
      stop("Duplicated rows in 'ParameterEstimate' slot")
    }
    return(TRUE)
  }
)
