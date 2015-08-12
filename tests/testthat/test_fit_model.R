context("fit_model")
describe("fit_model() on GlmerPoisson based objects", {
  temp.dir <- tempdir()
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  object <- n2k_glmer_poisson(
    scheme.id = 1L,
    species.group.id = 2L,
    location.group.id = 3L,
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990L,
    last.imported.year = 2015L,
    analysis.date = Sys.time(),
    data = cbpp
  )
  weighted.object <- n2k_glmer_poisson(
    scheme.id = 1L,
    species.group.id = 2L,
    location.group.id = 3L,
    model.type = "weighted glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990L,
    last.imported.year = 2015L,
    analysis.date = Sys.time(),
    data = cbpp
  )
  object.fit <- fit_model(object)
  weighted.object.fit <- fit_model(weighted.object)
  it("returns valid objects", {
    expect_that(
      validObject(object.fit),
      is_true()
    )
    expect_that(
      validObject(weighted.object.fit),
      is_true()
    )
  })
  it("works with objects saved in rda files", {
    analysis <- object
    filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
    save(analysis, file = filename)
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    expect_identical(status(filename)$Status, "converged")
    analysis <- weighted.object
    filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
    save(analysis, file = filename)
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    expect_identical(status(filename)$Status, "converged")
  })
})
