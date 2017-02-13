data("cbpp", package = "lme4")
cbpp$DatasourceID <- sha1(letters)
cbpp$ObservationID <- seq_len(nrow(cbpp))
object <- n2k_glmer_poisson(
  result.datasource.id = sha1(letters),
  scheme.id = sha1(letters),
  species.group.id = sha1(letters),
  location.group.id = sha1(letters),
  model.type = "glmer poisson: period + herd",
  formula = "incidence ~ offset(log(size)) + period + (1|herd)",
  first.imported.year = 1990,
  last.imported.year = 2015,
  analysis.date = as.POSIXct("2000-01-01 12:13:14", tz = "UTC"),
  data = cbpp
)

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
