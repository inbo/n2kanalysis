data("cbpp", package = "lme4")
cbpp$Count <- cbpp$incidence
object <- n2k_glmer_poisson(
  scheme.id = 1,
  species.group.id = 2,
  location.group.id = 3,
  model.type = "glmer poisson: period + herd",
  covariate = "offset(log(size)) + period + (1|herd)",
  first.imported.year = 1990,
  analysis.date = as.POSIXct("2000-01-01"),
  data = cbpp
)

context("get_weight() handles n2kGlmerPoisson objects")
describe("get_weight", {
  it("returns the Weight slot", {
    expect_that(
      get_weight(object),
      is_identical_to(object@Weight)
    )
  })
})

context("get_model() handles n2kGlmerPoisson objects")
describe("get_model", {
  model.object <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = object@Data,
    family = poisson
  )
  object.model <- n2k_glmer_poisson(
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
