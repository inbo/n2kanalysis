context("select data based the average")
describe("select_factor_threshold", {
  observation <- data.frame(
    Count = c(100, 101, 50, 51, 1, 0, 0, 0),
    location_id = rep(1:4, each = 2)
  )
  observation$location_factor <- factor(observation$location_id)
  variable <- "location_factor"
  variable_numeric <- "location_id"
  threshold <- 0.05

  it("selects correctly", {
    expect_that(
      suppressWarnings(
        select_factor_threshold(
          observation = observation,
          variable = variable,
          threshold = threshold
        )
      ),
      is_identical_to(subset(observation, location_id %in% 1:2))
    )
  })
  it("checks the number of observations", {
    expect_that(
      select_factor_threshold(
        observation = head(observation, -1),
        variable = variable,
        threshold = threshold
      ),
      throws_error(
        paste(
"The number of observations much be at least twice the number of levels in",
variable
        )
      )
    )
  })
})
