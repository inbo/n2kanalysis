context("select data based the observed range")
describe("select_observed_range", {
  observation <- data.frame(
    Count = c(0, 0, 100, 101, 0, 51, 1, 0, 0, 0),
    Year = 1:10
  )
  variable <- "Year"

  it("selects correctly", {
    expect_that(
      select_observed_range(
        observation = observation,
        variable = variable
      ),
      is_identical_to(observation[3:7, ])
    )
  })
  it("warns when the variable contains NA", {
    observation$Year[5] <- NA
    expect_that(
      select_observed_range(
        observation = observation,
        variable = variable
      ),
      gives_warning(
        paste(
          variable,
          "contains missing values. Corresponding rows are removed."
        )
      )
    )
  })
})
