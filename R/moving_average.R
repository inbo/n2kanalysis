#' Calculate moving trend coefficients
#' @inheritParams moving_trend
#' @return A matrix with moving average coefficients
#' One row for each window and and one column for each year in the data.
#' The format of the row names is `average_{window mid point}_{window length}`.
#' `average_2001.5_4` is the average for the years 2000 to 2003.
#' @export
#' @examples
#' moving_average(5, 3)
#' moving_average(5, 3, 2000)
#' @importFrom assertthat assert_that is.count is.number
moving_average <- function(n_year, duration, first_year = 0) {
  assert_that(is.count(n_year), is.count(duration), is.number(first_year))
  duration <- min(n_year, duration)
  vapply(
    seq_len(n_year - duration + 1) - 1,
    FUN.VALUE = vector(mode = "numeric", length = n_year),
    FUN = function(i, trend_coef, n_year) {
      c(rep(0, i), trend_coef, rep(0, n_year - length(trend_coef) - i))
    }, trend_coef = rep(1 / duration, duration), n_year = n_year
  ) |>
    `colnames<-`(
      sprintf(
        "average_%.1f_%i",
        seq_len(n_year - duration + 1) + median(seq_len(duration)) - 2 +
          first_year,
        duration
      )
    ) |>
    t()
}
