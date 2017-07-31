#' Select the observations based on the average of a factor
#'
#' The negative binomial average of the \code{Count} variable is calculated for each level of \code{variable}. Only the levels which are equal or larger than \code{treshold} times the maximal average (in the original scale) are retained.
#' @param observation the \code{data.frame} with observations
#' @param variable the name of the \code{factor}
#' @param treshold the minimal treshold
#' @export
#' @importFrom MASS glm.nb
#' @importFrom n2khelper check_single_probability check_dataframe_variable
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom stats coef
#' @examples
#' observation <- data.frame(
#'   Count = c(100, 101, 50, 51, 1, 0, 0, 0),
#'   LocationID = factor(rep(1:4, each = 2))
#' )
#' select_factor_treshold(observation, "LocationID", treshold = 0.05)
select_factor_treshold <- function(observation, variable, treshold){
  assert_that(is.string(variable))
  treshold <- check_single_probability(x = treshold, name = "treshold")
  assert_that(inherits(observation, "data.frame"))
  assert_that(has_name(observation, "Count"))
  assert_that(has_name(observation, variable))

  variable_factor <- observation[[variable]]
  if (!is.factor(variable_factor)) {
    variable_factor <- factor(variable_factor)
  }
  if (nrow(observation) < 2 * length(levels(variable_factor))) {
    stop(
"The number of observations much be at least twice the number of levels in ",
variable
    )
  }

  model <- glm.nb(observation$Count ~ 0 + variable_factor)
  log_treshold <- max(coef(model)) + log(treshold)
  selection <- levels(variable_factor)[coef(model) >= log_treshold]
  observation <- observation[variable_factor %in% selection, ]
  return(observation)
}
