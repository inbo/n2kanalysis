#' Select data based on the number of prescences per category
#'
#' Prescences have \eqn{Count > 0}.
#' @inheritParams select_factor_threshold
#' @param dimension Indicates which element of \code{variable} is used for the
#' final aggregation.
#' @param relative When FALSE the threshold is the number of non-zero
#' observations.
#' When TRUE the threshold is the proportion of non-zero observations.
#' Defaults to FALSE.
#' @export
#' @importFrom n2khelper check_character
#' @importFrom assertthat assert_that is.count is.flag is.number has_name noNA
#' @importFrom stats na.fail
#' @examples
#' observation <- data.frame(
#'   Count = c(4, 4, 4, 4, 3, 3, 3, 0, 2, 2, 0, 0),
#'   LocationID = rep(1:3, each = 4),
#'   Year = rep(c(1, 1, 1, 1, 2, 2), 2)
#' )
#' # Select the locations with at least 3 prescenses
#' select_factor_count_strictly_positive(
#'   observation,
#'   variable = "LocationID",
#'   threshold = 3
#' )
#' # Select those locations in which the species is present in at least 2 years
#' select_factor_count_strictly_positive(
#'   observation, variable = c("LocationID", "Year"), threshold = 2
#' )
#' # Select those years in which the species is present in at least 2 locations
#' select_factor_count_strictly_positive(
#'   observation, variable = c("LocationID", "Year"),
#'   threshold = 2,
#'   dimension = 2
#' )
select_factor_count_strictly_positive <- function(# nolint
  observation,
  variable,
  threshold,
  relative = FALSE,
  dimension = 1
) {
  variable <- check_character(
    x = variable,
    name = "variable",
    na_action = na.fail
  )
  assert_that(is.count(dimension))
  assert_that(is.flag(relative))
  assert_that(noNA(relative))
  if (relative && dimension > 1) {
    stop("relative threshold is only defined for 1 dimension")
  }
  if (relative) {
    assert_that(is.number(threshold), 0 < threshold, threshold < 1)
  } else {
    assert_that(is.count(threshold))
  }
  assert_that(inherits(observation, "data.frame"))
  assert_that(has_name(observation, "Count"))
  assert_that(all(has_name(observation, variable)))
  if (dimension > length(variable)) {
    stop("the dimension can't exceed the number of variables")
  }

  positive_observation <- observation[observation$Count > 0, ]
  observed_combination <- table(positive_observation[, variable])
  if (length(variable) == 1) {
    if (relative) {
      observed_combination <- observed_combination / sum(observed_combination)
    }
    relevance <- observed_combination >= threshold
  } else {
    relevance <- apply(observed_combination > 0, dimension, sum) >= threshold
  }
  selected_level <- names(relevance)[relevance]

  selection <- as.character(observation[[variable[dimension]]]) %in%
    selected_level
  return(observation[selection, ])
}
