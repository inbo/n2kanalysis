#' Calculate moving trend coefficients
#' @param n_year Number of available years in the data.
#' @param duration Number of years in the moving window.
#' If the number of available years is less than this value, the trend will be
#' calculated for the available years.
#' @param first_year First year of the data.
#' Only used to name the rows.
#' @return A matrix with the moving trend coefficients.
#' One row for each window and and one column for each year in the data.
#' The format of the row names is `trend_{window mid point}_{window length}`.
#' `trend_2001.5_4` is the trend for the years 2000 to 2003.
#' @export
#' @examples
#' moving_trend(5, 3)
#' moving_trend(5, 4, 2000)
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom stats median
moving_trend <- function(n_year, duration, first_year = 0) {
  assert_that(is.count(n_year), is.count(duration), is.number(first_year))
  duration <- min(n_year, duration)
  trend_coef <- seq_len(duration) - (duration + 1) / 2
  vapply(
    seq_len(n_year - duration + 1),
    function(i, trend_coef) {
      c(rep(0, i - 1), trend_coef, rep(0, n_year - duration - i + 1))
    },
    numeric(n_year), trend_coef = trend_coef / sum(trend_coef ^ 2)
  ) |>
    `colnames<-`(
      sprintf(
        "trend_%.1f_%i",
        seq_len(n_year - duration + 1) + median(seq_len(duration)) - 2 +
          first_year,
        duration
      )
    ) |>
    t()
}
