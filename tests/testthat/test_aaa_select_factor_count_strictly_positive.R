context("select data based on positive observations")
describe("select_factor_count_strictly_positive", {
  observation <- data.frame(
    Count = c(4, 4, 4, 4, 3, 3, 3, 0, 2, 2, 0, 0),
    LocationID = rep(1:3, each = 4),
    Year = rep(c(1, 1, 1, 1, 2, 2), 2)
  )
  observation_relative <- data.frame(
    Count = rep(1, 11),
    Year = c(rep(1, 10), 2)
  )

  it("selects correctly", {
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        threshold = 3,
        dimension = 1
      ),
      is_identical_to(subset(observation, LocationID %in% 1:2))
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = c("LocationID", "Year"),
        threshold = 2,
        dimension = 1
      ),
      is_identical_to(subset(observation, LocationID == 2))
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = c("LocationID", "Year"),
        threshold = 2,
        dimension = 2
      ),
      is_identical_to(subset(observation, Year == 1))
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation_relative,
        variable = "Year",
        threshold = 0.15,
        dimension = 1,
        relative = TRUE
      ),
      is_identical_to(subset(observation_relative, Year == 1))
    )
  })
  it("checks the number of dimensions", {
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        threshold = 3,
        dimension = 2
      ),
      throws_error("the dimension can't exceed the number of variables")
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = c("LocationID", "Year"),
        threshold = 3,
        dimension = 2,
        relative = TRUE
      ),
      throws_error("relative threshold is only defined for 1 dimension")
    )
  })
  it("checks the correct class of threshold", {
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        threshold = 0.15,
        dimension = 1,
        relative = FALSE
      ),
      throws_error("threshold is not a count \\(a single positive integer\\)")
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        threshold = 3,
        dimension = 1,
        relative = TRUE
      ),
      throws_error("threshold not less than 1")
    )
  })
})
