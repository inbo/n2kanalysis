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
#' @importFrom dplyr %>% summarise
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kParameter",
  function(object) {
    Parameter <- object@Parameter
    assert_that(
      has_name(Parameter, "Description"),
      has_name(Parameter, "Parent"),
      has_name(Parameter, "Fingerprint")
    )

    ParameterEstimate <- object@ParameterEstimate
    assert_that(
      has_name(ParameterEstimate, "Analysis"),
      has_name(ParameterEstimate, "Parameter"),
      has_name(ParameterEstimate, "Estimate"),
      has_name(ParameterEstimate, "LowerConfidenceLimit"),
      has_name(ParameterEstimate, "UpperConfidenceLimit")
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

    if (nrow(object@ParameterEstimate) > 0) {
      test <- object@ParameterEstimate %>%
        summarise(
          TestLCL = any(
            .data$Estimate - .data$LowerConfidenceLimit <
              -.Machine$double.neg.eps,
            na.rm = TRUE
          ),
          TestUCL = any(
            .data$Estimate - .data$UpperConfidenceLimit >
              .Machine$double.neg.eps,
            na.rm = TRUE
          )
        )
      if (test$TestLCL) {
        stop(
  "All Estimate in 'ParameterEstimate' slot must be greather than the
  LowerConfidenceLimit"
        )
      }
      if (test$TestUCL) {
        stop(
  "All Estimate in 'ParameterEstimate' slot must be less than the
  UpperConfidenceLimit"
        )
      }
    }
    return(TRUE)
  }
)
