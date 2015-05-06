
context("get_weight() handles n2kGlmerPoisson objects")
describe("get_weight", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    data = cbpp
  )
  it("returns the Weight slot", {
    expect_that(
      get_weight(object),
      is_identical_to(object@Weight)
    )
  })
})

context("get_model() handles n2kGlmerPoisson objects")
describe("get_model", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    data = cbpp
  )
  it("returns the Model slot", {
    expect_that(
      get_model(object),
      is_identical_to(object@Model)
    )
  })
})
