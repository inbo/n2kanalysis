context("select data based the average")
describe("select_factor_treshold", {
  observation <- data.frame(
    Count = c(100, 101, 50, 51, 1, 0, 0, 0),
    LocationID = rep(1:4, each = 2)
  )
  observation$LocationFactor <- factor(observation$LocationID)
  variable <- "LocationFactor"
  variable.numeric <- "LocationID"
  treshold <- 0.05

  it("selects correctly", {
    expect_that(
      select_factor_treshold(
        observation = observation,
        variable = variable,
        treshold = treshold
      ),
      is_identical_to(subset(observation, LocationID %in% 1:2))
    )
  })
  it("checks the number of observations", {
    expect_that(
      select_factor_treshold(
        observation = head(observation, -1),
        variable = variable,
        treshold = treshold
      ),
      throws_error(
        paste(
"The number of observations much be at least twice the number of levels in",
variable
        )
      )
    )
  })
  it("warns when the variable is not a factor", {
    expect_that(
      select_factor_treshold(
        observation = observation,
        variable = variable.numeric,
        treshold = treshold
      ),
      gives_warning(paste(variable.numeric, "was converted to a factor"))
    )
  })
})
