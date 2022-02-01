#' The n2kContrast class
#' @name n2kContrast-class
#' @rdname n2kContrast-class
#' @exportClass n2kContrast
#' @aliases n2kContrast-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kContrast",
  representation = representation(
    Contrast = "data.frame",
    ContrastCoefficient = "data.frame",
    ContrastEstimate = "data.frame"
  ),
  prototype = prototype(
    Contrast = data.frame(
      fingerprint = character(0), description = character(0),
      analysis = character(0), stringsAsFactors = FALSE
    ),
    ContrastCoefficient = data.frame(
      contrast = character(0), parameter = character(0),
      coefficient = numeric(0), stringsAsFactors = FALSE
    ),
    ContrastEstimate = data.frame(
      contrast = character(0), estimate = numeric(0),
      lower_confidence_limit = numeric(0), upper_confidence_limit = numeric(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kContrast",
  function(object) {
    check_dataframe_variable(
      df = object@Contrast,
      variable = c("fingerprint", "description", "analysis"),
      name = "Contrast"
    )
    check_dataframe_variable(
      df = object@ContrastCoefficient,
      variable = c("contrast", "parameter", "coefficient"),
      name = "ContrastCoefficient"
    )
    check_dataframe_variable(
      df = object@ContrastEstimate,
      variable = c(
        "contrast", "estimate", "lower_confidence_limit",
        "upper_confidence_limit"
      ),
      name = "ContrastEstimate"
    )
    if (!all(
      na.omit(object@ContrastCoefficient$contrast) %in%
        object@Contrast$fingerprint
    )) {
      stop("Some contrast in 'ConstrastCoefficient' slot not found")
    }
    if (!all(
      na.omit(object@ContrastEstimate$contrast) %in%
      object@Contrast$fingerprint
    )) {
      stop("Some contrast in 'ConstrastEstimate' slot not found")
    }

    if (anyDuplicated(object@Contrast$fingerprint)) {
      stop("Duplicated fingerprint in 'Contrast' slot")
    }
    if (anyDuplicated(
      object@Contrast[, c("description", "analysis")]
    )) {
      stop("Duplicated rows in 'Contrast' slot")
    }
    if (anyDuplicated(
      object@ContrastCoefficient[, c("contrast", "parameter")]
    )) {
      stop("Duplicated rows in 'ContrastCoefficient' slot")
    }
    if (anyDuplicated(object@ContrastEstimate$contrast)) {
      stop("Duplicated contrast in 'ContrastEstimate' slot")
    }
    return(TRUE)
  }
)
