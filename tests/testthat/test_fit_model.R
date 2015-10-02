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
    "\nobject.file <- \"", get_file_fingerprint(object), "\"\n",
    "weighted.object.file <- \"",
      get_file_fingerprint(weighted.object), "\"\n",
    sep = ""
  )
  # 32-bit windows
  object.file <- "e82823bfa475cb8a2e56299c82ebcd56c2f637c6"
  weighted.object.file <- "e81b8fef2b8f0d7a988ba64a478d7e7db213d217"

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

describe("fit_model() on INLA nbinomial based objects", {
  temp.dir <- tempdir()
  data(cbpp, package = "lme4")
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  cbpp$DatasourceID <- 2L
  this.analysis.date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this.scheme.id <- 1L
  this.species.group.id <- 2L
  this.location.group.id <- 3L
  this.model.type <- "inla nbinomial: period + herd"
  this.formula <-
    "incidence ~ offset(log(size)) + period + f(herd, model = 'iid')"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  lin.comb <- model.matrix(~period, unique(cbpp[, "period", drop = FALSE]))
  bad.lin.comb <- lin.comb[, -1]
  object <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp
  )
  object.lc <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp,
    lin.comb = lin.comb
  )
  object.badlc <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp,
    lin.comb = bad.lin.comb
  )
  object.fit <- fit_model(object)
  object.lc.fit <- fit_model(object.lc)
  object.badlc.fit <- fit_model(object.badlc)
  cat(
    "\nobject.file <- \"", get_file_fingerprint(object), "\"\n",
    "object.lc.file <- \"", get_file_fingerprint(object.lc), "\"\n",
    "object.badlc.file <- \"", get_file_fingerprint(object.badlc), "\"\n",
    sep = ""
  )
  # 32-bit windows
  object.file <- "baa8c09603d73b5d83787f7470c483ec5d4cca47"
  object.lc.file <- "324e8c4868d4f786b37a8045461222527cda1325"
  object.badlc.file <- "c9bffef83ce581835781f32def67cd616aaabaa9"

  it("returns the same file fingerprints on 32-bit and 64-bit", {
    expect_identical(object.file, get_file_fingerprint(object))
    expect_identical(object.lc.file, get_file_fingerprint(object.lc))
  })
  it("doesn't alter the file fingerprint when fitting a model", {
    expect_identical(
      get_file_fingerprint(object),
      get_file_fingerprint(object.fit)
    )
    expect_identical(
      get_file_fingerprint(object.lc),
      get_file_fingerprint(object.lc.fit)
    )
  })
  it("returns valid objects", {
    expect_that(
      validObject(object.fit),
      is_true()
    )
    expect_that(
      validObject(object.lc.fit),
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
    analysis <- object.lc
    filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
    save(analysis, file = filename)
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    expect_identical(status(filename)$Status, "converged")
  })

  it("doesn't refit converged models with the default status", {
    expect_identical(
      fit_model(object.fit),
      object.fit
    )
    expect_identical(
      fit_model(object.lc.fit),
      object.lc.fit
    )
  })
  it("returns an error when the linear combination is not valid", {
    expect_identical(
      status(object.badlc.fit),
      "error"
    )
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
