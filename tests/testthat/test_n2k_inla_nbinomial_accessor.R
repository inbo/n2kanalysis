context("get_model() handles n2kInlaNbinomial objects")
describe("get_model", {
  require(INLA)
  data("cbpp", package = "lme4")
  cbpp$Count <- cbpp$incidence
  object <- n2k_inla_nbinomial(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "inla nbinomial: period + herd",
    covariate = "offset(log(size)) + period + f(herd, model = 'iid')",
    first.imported.year = 1990,
    last.imported.year = 2015,
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  model.object <- INLA::inla(
    Count ~ offset(log(size)) + period + f(herd, model = "iid"), 
    data = object@Data,
    family = "nbinomial"
  )
  object.model <- n2k_inla_nbinomial(
    data = object, model.fit = model.object, status = "converged"
  )
  it("returns the Model slot", {
    expect_that(
      get_model(object),
      is_identical_to(object@Model)
    )
  })
  it("returns the Model slot", {
    expect_that(
      get_model(object.model),
      is_identical_to(object.model@Model)
    )
  })
})
