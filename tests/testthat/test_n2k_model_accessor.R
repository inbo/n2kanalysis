context("n2kModel accesor functions")

context("status() handles n2kModel objects")
describe("status", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    data = cbpp
  )
  model.object <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = object@Data,
    family = poisson
  )
  object.model <- n2k_glmer_poisson(
    data = object, model.fit = model.object, status = "converged"
  )
  it("returns the status", {
    expect_that(
      status(object),
      is_identical_to(object@Status)
    )
    expect_that(
      status(object.model),
      is_identical_to(object.model@Status)
    )
  })
  it("updates the status", {
    status(object) <- "error"
    expect_that(
      status(object),
      is_identical_to("error")
    )
    expect_that(
      status(object) <- "junk",
      throws_error("Status must be one of the following")
    )
  })
})

context("get_data() handles n2k_virtual objects")
describe("get_data", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    data = cbpp
  )
  model.object <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = object@Data,
    family = poisson
  )
  object.model <- n2k_glmer_poisson(
    data = object, model.fit = model.object, status = "converged"
  )
  it("returns the Data slot", {
    expect_that(
      get_data(object),
      is_identical_to(cbpp)
    )
    expect_that(
      get_data(object.model),
      is_identical_to(cbpp)
    )
  })
})


context("get_seed() handles n2k_virtual objects")
describe("get_seed", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    data = cbpp
  )
  it("returns the Seed slot", {
    expect_that(
      get_seed(object),
      is_identical_to(object@Seed)
    )
  })
})
