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
      Fingerprint = character(0),
      Description = character(0),
      Analysis = character(0),
      stringsAsFactors = FALSE
    ),
    ContrastCoefficient = data.frame(
      Contrast = character(0),
      Parameter = character(0),
      Coefficient = numeric(0),
      stringsAsFactors = FALSE
    ),
    ContrastEstimate = data.frame(
      Contrast = character(0),
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
  "n2kContrast",
  function(object) {
    check_dataframe_variable(
      df = object@Contrast,
      variable = c("Fingerprint", "Description", "Analysis"),
      name = "Contrast"
    )
    check_dataframe_variable(
      df = object@ContrastCoefficient,
      variable = c("Contrast", "Parameter", "Coefficient"),
      name = "ContrastCoefficient"
    )
    check_dataframe_variable(
      df = object@ContrastEstimate,
      variable = c(
        "Contrast", "Estimate", "LowerConfidenceLimit",
        "UpperConfidenceLimit"
      ),
      name = "ContrastEstimate"
    )
    if (!all(
      na.omit(object@ContrastCoefficient$Contrast) %in%
        object@Contrast$Fingerprint
    )) {
      stop("Some Contrast in 'ConstrastCoefficient' slot not found")
    }
    if (!all(
      na.omit(object@ContrastEstimate$Contrast) %in%
      object@Contrast$Fingerprint
    )) {
      stop("Some Contrast in 'ConstrastEstimate' slot not found")
    }

    if (anyDuplicated(object@Contrast$Fingerprint)) {
      stop("Duplicated Fingerprint in 'Contrast' slot")
    }
    if (anyDuplicated(
      object@Contrast[, c("Description", "Analysis")]
    )) {
      stop("Duplicated rows in 'Contrast' slot")
    }
    if (anyDuplicated(
      object@ContrastCoefficient[, c("Contrast", "Parameter")]
    )) {
      stop("Duplicated rows in 'ContrastCoefficient' slot")
    }
    if (anyDuplicated(object@ContrastEstimate$Contrast)) {
      stop("Duplicated Contrast in 'ContrastEstimate' slot")
    }
    return(TRUE)
  }
)
