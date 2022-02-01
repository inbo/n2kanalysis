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
      description = character(0), parent = character(0),
      fingerprint = character(0), stringsAsFactors = FALSE
    ),
    ParameterEstimate = data.frame(
      analysis = character(0), parameter = character(0), estimate = numeric(0),
      lower_confidence_limit = numeric(0), upper_confidence_limit = numeric(0),
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
    parameter <- object@Parameter
    assert_that(
      has_name(parameter, "description"), has_name(parameter, "parent"),
      has_name(parameter, "fingerprint")
    )

    parameter_estimate <- object@ParameterEstimate
    assert_that(
      has_name(parameter_estimate, "analysis"),
      has_name(parameter_estimate, "parameter"),
      has_name(parameter_estimate, "estimate"),
      has_name(parameter_estimate, "lower_confidence_limit"),
      has_name(parameter_estimate, "upper_confidence_limit")
    )

    if (!all(
      na.omit(object@Parameter$parent) %in% object@Parameter$fingerprint
    )) {
      stop("Some parent in 'Parameter' slot not found")
    }
    if (!all(
      object@ParameterEstimate$parameter %in% object@Parameter$fingerprint
    )) {
      stop(
"Some parameter in 'ParameterEstimate' slot have no matching fingerprint in
'Parameter' slot"
      )
    }
    if (anyDuplicated(object@Parameter$fingerprint)) {
      stop("Duplicated fingerprint in 'Parameter' slot")
    }
    if (anyDuplicated(object@Parameter[, c("description", "parent")])) {
      stop("Duplicated rows in 'Parameter' slot")
    }
    if (anyDuplicated(object@ParameterEstimate[, c("analysis", "parameter")])) {
      stop("Duplicated rows in 'ParameterEstimate' slot")
    }

    if (nrow(object@ParameterEstimate) > 0) {
      test <- object@ParameterEstimate %>%
        summarise(
          test_lcl = any(
            .data$estimate - .data$lower_confidence_limit <
              -.Machine$double.neg.eps,
            na.rm = TRUE
          ),
          test_ucl = any(
            .data$estimate - .data$upper_confidence_limit >
              .Machine$double.neg.eps,
            na.rm = TRUE
          )
        )
      if (test$test_lcl) {
        stop(
  "All estimate in 'ParameterEstimate' slot must be greather than the
  lower_confidence_limit"
        )
      }
      if (test$test_ucl) {
        stop(
  "All estimate in 'ParameterEstimate' slot must be less than the
  upper_confidence_limit"
        )
      }
    }
    return(TRUE)
  }
)
