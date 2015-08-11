#' Select the observations based on the average of a factor
#' 
#' The negative binomial average of the \code{Count} variable is calculated for each level of \code{variable}. Only the levels which are equal or larger than \code{threshold} times the maximal average (in the original scale) are retained.
#' @param observation the \code{data.frame} with observations
#' @param variable the name of the \code{factor}
#' @param threshold the minimal threshold
#' @export
#' @importFrom MASS glm.nb
#' @importFrom n2khelper check_single_probability check_dataframe_variable
#' @importFrom assertthat assert_that is.string
#' @examples
#' observation <- data.frame(
#'   Count = c(100, 101, 50, 51, 1, 0, 0, 0),
#'   LocationID = factor(rep(1:4, each = 2))
#' )
#' select_factor_threshold(observation, "LocationID", threshold = 0.05)
select_factor_threshold <- function(observation, variable, threshold){
  assert_that(is.string(variable))
  threshold <- check_single_probability(x = threshold, name = "threshold")
  junk <- check_dataframe_variable(
    df = observation, 
    variable = c("Count", variable), 
    name = "observation", 
    error = TRUE
  )
  
  if (class(observation[, variable]) == "factor") {
    variable.factor <- observation[, variable]
  } else {
    variable.factor <- factor(observation[, variable])
    warning(variable, " was converted to a factor")
  }
  if (nrow(observation) < 2 * length(levels(variable.factor))) {
    stop("The number of observations much be at least twice the number of levels in ", variable)
  }
  
  model <- glm.nb(observation$Count ~ 0 + variable.factor)
  log.threshold <- max(coef(model)) + log(threshold)
  selection <- levels(variable.factor)[coef(model) >= log.threshold]
  observation <- observation[variable.factor %in% selection, ]
  return(observation)
}
