context("fit_model")
describe("fit_model() on GlmerPoisson based objects", {
  temp.dir <- tempdir()
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this.seed <- 1L
  object <- n2k_glmer_poisson(
    result.datasource.id = sha1(letters),
    scheme.id = sha1(letters),
    species.group.id = sha1(letters),
    location.group.id = sha1(letters),
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990L,
    last.imported.year = 2015L,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp
  )
  weighted.object <- n2k_glmer_poisson(
    result.datasource.id = sha1(letters),
    scheme.id = sha1(letters),
    species.group.id = sha1(letters),
    location.group.id = sha1(letters),
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
  object.file <- "fc1c87efe0011e8d306fd9c5d4c1b8fecc67c94c"
  weighted.object.file <- "af640ba204e5f9268a26d0e8570ebeb0e1048506"

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
  it("works with objects saved in rds files", {
    filename <- store_model(object, base = temp.dir, project = "fit_model")
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
    filename <- store_model(
      weighted.object,
      base = temp.dir,
      project = "fit_model"
    )
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
  })

  # clean temp files
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
})

describe("fit_model() on INLA nbinomial based objects", {
  temp.dir <- tempdir()
  dataset <- test_data(missing = 0.2)
  this.analysis.date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this.result.datasource.id <- sha1(letters)
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.model.type <- "inla nbinomial: A + B + C + D"
  this.formula <-
    "Count ~ A * (B + C) + C * D + f(E, model = 'iid')"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  lin.comb <- dataset %>%
    distinct_(~A) %>%
    model.matrix(object = ~A)
  rownames(lin.comb) <- seq_len(nrow(lin.comb))
  bad.lin.comb <- lin.comb[, -1]
  lin.comb.list <- as.list(as.data.frame(lin.comb))
  names(lin.comb.list[[1]]) <- seq_along(lin.comb.list[[1]])
  lin.comb.list2 <- list(E = diag(length(unique(dataset$E))))
  rownames(lin.comb.list2[[1]]) <- seq_along(unique(dataset$E))
  object <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset
  )
  object.lc <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = lin.comb
  )
  object.lc.list <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = lin.comb.list
  )
  object.lc.list2 <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = lin.comb.list2
  )
  object.badlc <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = bad.lin.comb
  )
  object.imp <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    imputation.size = 10,
    data = dataset
  )
  timeout <- fit_model(object, timeout = 0.001)
  expect_identical(status(timeout), "time-out")
  object.fit <- fit_model(object)
  object.lc.fit <- fit_model(object.lc)
  object.lc.list.fit <- fit_model(object.lc.list)
  object.lc.list2.fit <- fit_model(object.lc.list2)
  object.badlc.fit <- fit_model(object.badlc)
  object.imp.fit <- fit_model(object.imp)
  cat(
    "\nobject.file <- \"", get_file_fingerprint(object), "\"\n",
    "object.lc.file <- \"", get_file_fingerprint(object.lc), "\"\n",
    "object.lc.list.file <- \"", get_file_fingerprint(object.lc.list), "\"\n",
    "object.lc.list2.file <- \"", get_file_fingerprint(object.lc.list2), "\"\n",
    "object.badlc.file <- \"", get_file_fingerprint(object.badlc), "\"\n",
    sep = ""
  )
  # 32-bit windows
  object.file <- "c003f3ef708b55810e9645cb629724a1c6bed197"
  object.lc.file <- "480013538a26ded79a2da5c1dc18bd42434a4830"
  object.lc.list.file <- "8e1e94bdb5569a1aaadfe97d79a5edb322eeecb8"
  object.lc.list2.file <- "e92be956a1d53b577267e4e2d8294cb79e79b590"
  object.badlc.file <- "c3d7da408a78be3282b3677153357db26b5d6bca"
  it("returns the same file fingerprints on 32-bit and 64-bit", {
    expect_identical(object.file, get_file_fingerprint(object))
    expect_identical(object.lc.file, get_file_fingerprint(object.lc))
    expect_identical(object.lc.list.file, get_file_fingerprint(object.lc.list))
    expect_identical(
      object.lc.list2.file,
      get_file_fingerprint(object.lc.list2)
    )
    expect_identical(object.badlc.file, get_file_fingerprint(object.badlc))
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
  it("works with objects saved in rds files", {
    analysis <- object
    filename <- store_model(analysis, base = temp.dir, project = "fit_model")
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
    analysis <- object.lc
    filename <- store_model(analysis, base = temp.dir, project = "fit_model")
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
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
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
})

test_that("fit_model() works on n2kInlaComparison", {
  this.result.datasource.id <- sha1(letters)
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.analysis.date <- Sys.time()
  this.model.type <- "inla nbinomial: A * (B + C) + C:D"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  dataset <- test_data()
  temp.dir <- tempdir()

  analysis <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = "Count ~ A",
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset
  )
  p1 <- get_file_fingerprint(analysis)
  filename1 <- store_model(analysis, base = temp.dir, project = "fit_model")
  analysis <- n2k_inla_nbinomial(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = "Count ~ A * B",
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset
  )
  p2 <- get_file_fingerprint(analysis)
  filename2 <- store_model(analysis, base = temp.dir, project = "fit_model")

  analysis <- n2k_inla_comparison(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    formula = "~B", #nolint
    model.type = "inla comparison: A*B",
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    parent = c(p1, p2),
    parent.status = status(temp.dir) %>%
      select_(
        ParentAnalysis = ~FileFingerprint,
        ParentStatus = ~Status,
        ParentStatusFingerprint = ~StatusFingerprint
      )
  )
  filename3 <- store_model(analysis, base = temp.dir, project = "fit_model")
  fit_model(
    get_file_fingerprint(analysis),
    base = temp.dir,
    project = "fit_model",
    verbose = FALSE
  )

  fit_model(filename1, verbose = FALSE)
  fit_model(filename3, verbose = FALSE)

  fit_model(filename2, verbose = FALSE)
  fit_model(filename3, verbose = FALSE)
  filename3 <- gsub("waiting", "converged", filename3)
  expect_identical(
    fit_model(filename3, verbose = FALSE),
    NULL
  )

  # clean temp files
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
})

test_that("fit_model() works in n2kLrtGlmer objects", {
  this.seed <- 50L
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
  this.analysis.date <- Sys.time()
  this.model.type.parent <- "glmer poisson: period + herd"
  this.model.type <- "glmer lrt: cYear / fYear"
  this.formula <- "~ period"
  this.formula.0 <- "incidence ~ offset(log(size)) + period + (1|herd)"
  this.formula.1 <- "incidence ~ offset(log(size)) + (1|herd)"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  data("cbpp", package = "lme4")
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  object.1 <- n2k_glmer_poisson(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type.parent,
    formula = this.formula.1,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp
  )
  object.0 <- n2k_glmer_poisson(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type.parent,
    formula = this.formula.0,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp
  )
  this.parent.status <- data.frame(
    ParentAnalysis = c(
      get_file_fingerprint(object.1),
      get_file_fingerprint(object.0)
    ),
    ParentStatusFingerprint = c(
      get_status_fingerprint(object.1),
      get_status_fingerprint(object.0)
    ),
    ParentStatus = c(status(object.1), status(object.0)),
    stringsAsFactors = FALSE
  )
  x <- n2k_lrt_glmer(
    parent = get_file_fingerprint(object.1),
    parent.0 = get_file_fingerprint(object.0),
    parent.status = this.parent.status,
    seed = as.numeric(this.seed),
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date
  )
  temp.dir <- tempdir()
  project <- "lrtglmer"
  store_model(object.1, base = temp.dir, project = project)
  store_model(object.0, base = temp.dir, project = project)
  expect_is(
    fit_model(x, base = temp.dir, project = project),
    "n2kLrtGlmer"
  )
  # clean temp files
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
})
