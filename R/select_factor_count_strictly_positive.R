#' Select data based on the number of prescences per category
#'
#' Prescences have \eqn{Count > 0}.
#' @inheritParams select_factor_threshold
#' @param dimension indicates which element of \code{variable} is used for the final aggregation
#' @param relative When FALSE the threshold is the number of non-zero observations. When TRUE the threshold is the proportion of non-zero observations. Defaults to FALSE.
#' @export
#' @importFrom n2khelper check_character check_dataframe_variable check_single_probability
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom stats na.fail
#' @examples
#' observation <- data.frame(
#'   Count = c(4, 4, 4, 4, 3, 3, 3, 0, 2, 2, 0, 0),
#'   LocationID = rep(1:3, each = 4),
#'   Year = rep(c(1, 1, 1, 1, 2, 2), 2)
#' )
#' # Select the locations with at least 3 prescenses
#' select_factor_count_strictly_positive(observation, variable = "LocationID", threshold = 3)
#' # Select those locations in which the species is present in at least 2 years
#' select_factor_count_strictly_positive(
#'   observation, variable = c("LocationID", "Year"), threshold = 2
#' )
#' # Select those years in which the species is present in at least 2 locations
#' select_factor_count_strictly_positive(
#'   observation, variable = c("LocationID", "Year"), threshold = 2, dimension = 2
#' )
select_factor_count_strictly_positive <- function(
  observation,
  variable,
  threshold,
  relative = FALSE,
  dimension = 1
){
  variable <- check_character(
    x = variable,
    name = "variable",
    na.action = na.fail
  )
  assert_that(is.count(dimension))
  assert_that(is.flag(relative))
  if (relative && dimension > 1) {
    stop("relative threshold is only defined for 1 dimension")
  }
  if (relative) {
    threshold <- check_single_probability(x = threshold, name = "threshold")
  } else {
    assert_that(is.count(threshold))
  }
  check_dataframe_variable(
    df = observation,
    variable = c("Count", variable),
    name = "observation",
    error = TRUE
  )
  if (dimension > length(variable)) {
    stop("the dimension can't exceed the number of variables")
  }

  positive.observation <- observation[observation$Count > 0, ]
  observed.combination <- table(positive.observation[, variable])
  if (length(variable) == 1) {
    if (relative) {
      observed.combination <- observed.combination / sum(observed.combination)
    }
    relevance <- observed.combination >= threshold
  } else {
    relevance <- apply(observed.combination > 0, dimension, sum) >= threshold
  }
  selected.level <- names(relevance)[relevance]

  selection <- as.character(observation[, variable[dimension]]) %in%
    selected.level
  return(observation[selection, ])
}
