context("fit_model")
describe("fit_model() on GlmerPoisson based objects", {
  temp.dir <- tempdir()
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this.seed <- 1L
  object <- n2k_glmer_poisson(
    scheme.id = 1L,
    species.group.id = 2L,
    location.group.id = 3L,
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990L,
    last.imported.year = 2015L,
    analysis.date = this.analysis.date,
    seed = this.seed,
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
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp
  )
  object.fit <- fit_model(object)
  weighted.object.fit <- fit_model(weighted.object)
  cat(
    "object.file <- \"", get_file_fingerprint(object), "\"\n",
    "weighted.object.file <- \"",
      get_file_fingerprint(weighted.object), "\"\n",
    sep = ""
  )
  # 32-bit windows
  object.file <- "83105a866f2397ac0f42cf4e3b4f2edca581f003"
  weighted.object.file <- "a0eca2e2ae2642e1635c5128895598424d3ab254"

  it("returns the same file fingerprints on 32-bit and 64-bit", {
    expect_identical(object.file, get_file_fingerprint(object))
    expect_identical(
      weighted.object.file,
      get_file_fingerprint(weighted.object)
    )
  })
  it("doesn't alter the file fingerprint when fitting a model", {
    expect_identical(
      get_file_fingerprint(object),
      get_file_fingerprint(object.fit)
    )
    expect_identical(
      get_file_fingerprint(weighted.object),
      get_file_fingerprint(weighted.object.fit)
    )
  })
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

  # clean temp files
  file.remove(
    list.files(
      temp.dir,
      pattern = "^[0-9a-f]{40}\\.rda$",
      full.names = TRUE
    )
  )
})
