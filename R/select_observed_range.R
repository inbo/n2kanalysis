#' Limit the observation to the range in which the species is present
#' @inheritParams select_factor_threshold
#' @export
#' @examples
#' observation <- data.frame(
#'   Count = c(0, 0, 100, 101, 0, 51, 1, 0, 0, 0),
#'   Year = 1:10
#' )
#' select_observed_range(observation, "Year")
#' @importFrom n2khelper check_character check_dataframe_variable
#' @importFrom stats na.fail
select_observed_range <- function(observation, variable) {
  variable <- check_character(
    x = variable,
    name = "variable",
    na_action = na.fail
  )
  junk <- check_dataframe_variable(
    df = observation,
    variable = c("Count", variable),
    name = "observation",
    error = TRUE
  )
  if (any(is.na(observation[, variable]))) {
    warning(
      variable, " contains missing values. Corresponding rows are removed."
    )
  }

  observed.range <- range(
    observation[observation$Count > 0, variable],
    na.rm = TRUE
  )
  selection <- observed.range[1] <= observation[, variable] &
    observation[, variable] <= observed.range[2]
  return(observation[selection, ])
}
