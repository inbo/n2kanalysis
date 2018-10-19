context("get_model() handles n2kInla objects")
describe("get_model", {
  data("cbpp", package = "lme4")
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  object <- n2k_inla(
    result.datasource.id = sha1(letters),
    scheme.id = sha1(letters),
    species.group.id = sha1(letters),
    location.group.id = sha1(letters),
    model.type = "inla poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + f(herd, model = 'iid')",
    first.imported.year = 1990,
    last.imported.year = 2015,
    analysis.date = as.POSIXct("2000-01-01 12:13:14", tz = "UTC"),
    data = cbpp
  )
  model.object <- inla(
    incidence ~ offset(log(size)) + period + f(herd, model = "iid"),
    data = object@Data,
    family = "poisson"
  )
  object.model <- n2k_inla(
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
