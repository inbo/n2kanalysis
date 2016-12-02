context("select data based on positive observations")
describe("select_factor_count_strictly_positive", {
  observation <- data.frame(
    Count = c(4, 4, 4, 4, 3, 3, 3, 0, 2, 2, 0, 0),
    LocationID = rep(1:3, each = 4),
    Year = rep(c(1, 1, 1, 1, 2, 2), 2)
  )
  observation.relative <- data.frame(
    Count = rep(1, 11),
    Year = c(rep(1, 10), 2)
  )

  it("selects correctly", {
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        treshold = 3,
        dimension = 1
      ),
      is_identical_to(subset(observation, LocationID %in% 1:2))
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = c("LocationID", "Year"),
        treshold = 2,
        dimension = 1
      ),
      is_identical_to(subset(observation, LocationID == 2))
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = c("LocationID", "Year"),
        treshold = 2,
        dimension = 2
      ),
      is_identical_to(subset(observation, Year == 1))
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation.relative,
        variable = "Year",
        treshold = 0.15,
        dimension = 1,
        relative = TRUE
      ),
      is_identical_to(subset(observation.relative, Year == 1))
    )
  })
  it("checks the number of dimensions", {
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        treshold = 3,
        dimension = 2
      ),
      throws_error("the dimension can't exceed the number of variables")
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = c("LocationID", "Year"),
        treshold = 3,
        dimension = 2,
        relative = TRUE
      ),
      throws_error("relative treshold is only defined for 1 dimension")
    )
  })
  it("checks the correct class of treshold", {
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        treshold = 0.15,
        dimension = 1,
        relative = FALSE
      ),
      throws_error("treshold is not a count \\(a single positive integer\\)")
    )
    expect_that(
      select_factor_count_strictly_positive(
        observation = observation,
        variable = "LocationID",
        treshold = 3,
        dimension = 1,
        relative = TRUE
      ),
      throws_error("treshold must be smaller than 1")
    )
  })
})
