#' Calculate coefficients for a moving difference
#' @inheritParams moving_trend
#' @return A matrix with moving average coefficients
#' One row for each window and and one column for each year in the data.
#' The format of the row names is
#' `difference_{window mid point start}_{window mid point end}_{window length}`.
#' `difference_2001.5_2010.5_4` is the difference of the average for period for
#' the years 2009 to 2012 compared to the period from 2000 to 2003.
#' @export
#' @examples
#' moving_difference(6, 2)
#' moving_difference(6, 2, 2000)
#' @importFrom assertthat assert_that is.count is.number
moving_difference <- function(n_year, duration, first_year = 1) {
  assert_that(is.count(n_year), is.count(duration), is.number(first_year))
  duration <- min(floor(n_year / 2), duration)
  list(seq_len(n_year - 2 * duration + 1) - 1) |>
    rep(2) |>
    expand.grid() -> extra_zero
  extra_zero <- extra_zero[
    rowSums(extra_zero) <= n_year - 2 * duration,
  ]
  vapply(
    seq_len(nrow(extra_zero)),
    FUN.VALUE = vector(mode = "numeric", length = n_year),
    FUN = function(i, trend_coef, n_year, extra_zero) {
      c(
        rep(0, extra_zero[i, 1]), -trend_coef,
        rep(0, n_year - 2 * length(trend_coef) - sum(extra_zero[i, ])),
        trend_coef, rep(0, extra_zero[i, 2])
      )
    }, trend_coef = rep(1 / duration, duration), n_year = n_year,
    extra_zero = extra_zero
  ) |>
    `colnames<-`(
      sprintf(
        "difference_%.1f_%.1f_%i",
        extra_zero[, 1] + median(seq_len(duration)) - 1 + first_year,
        n_year - extra_zero[, 2] - median(seq_len(duration)) + first_year,
        duration
      )
    ) |>
    t()
}
