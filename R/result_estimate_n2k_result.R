#' @name result_estimate
#' @rdname result_estimate
#' @exportMethod result_estimate
#' @docType methods
#' @importFrom dplyr inner_join left_join select transmute
#' @importFrom methods setMethod slot
#' @importFrom rlang .data
setMethod(
  f = "result_estimate",
  signature = signature(x = "n2kResult"),
  definition = function(x, ...) {
    validObject(x)
    slot(x, "ParameterEstimate") |>
      inner_join(
        slot(x, "Parameter"), by = c("parameter" = "fingerprint")
      ) -> estimates
    while (any(!is.na(estimates$parent))) {
      estimates |>
        left_join(
          slot(x, "Parameter"), by = c("parent" = "fingerprint")
        ) |>
        transmute(
          .data$analysis, .data$estimate, .data$lower_confidence_limit,
          .data$upper_confidence_limit, parent = .data$parent.y,
          description = paste(
            .data$description.y, .data$description.x, sep = ":"
          )
        ) -> estimates
    }
    estimates |>
      select(-"parent")
  }
)
