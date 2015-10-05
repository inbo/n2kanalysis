context("get_result")
temp.dir <- tempdir()
this.scheme.id <- 1L
this.species.group.id <- 2L
this.location.group.id <- 3L
this.analysis.date <- Sys.time()
this.first.imported.year <- 1990L
this.last.imported.year <- 2015L
this.last.analysed.year <- 2014L
this.duration <- 1L
this.datasource <- 1L
data("cbpp", package = "lme4")
cbpp$ObservationID <- seq_len(nrow(cbpp))
cbpp$DatasourceID <- 2L
describe("get_result on n2kInlaNbinomial", {
  this.model.type <- "inla nbinomial: period + herd"
  this.formula <-
    "incidence ~ offset(log(size)) + period + f(herd, model = 'iid')"
  analysis <- n2k_inla_nbinomial(
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
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on unfitted objects", {
    expect_identical(
      nrow(result@Parameter),
      0L
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_identical(
      nrow(result@Anomaly),
      0L
    )
  })
  filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
  save(analysis, file = filename)
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )
  fit_model(filename)
  load(filename)
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_less_than(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_less_than(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )

  # with linear combination
  lin.comb <- model.matrix(~period, unique(cbpp[, "period", drop = FALSE]))
  this.parent <- "abcd"
  analysis <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp,
    lin.comb = lin.comb,
    parent = this.parent
  )
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on unfitted objects", {
    expect_identical(
      nrow(result@Parameter),
      0L
    )
    expect_identical(
      nrow(result@Contrast),
      nrow(lin.comb)
    )
    expect_identical(
      nrow(result@ContrastCoefficient),
      0L
    )
    expect_identical(
      nrow(result@ContrastEstimate),
      0L
    )
    expect_identical(
      nrow(result@Anomaly),
      0L
    )
  })
  filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
  save(analysis, file = filename)
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )
  fit_model(filename)
  load(filename)
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_less_than(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      nrow(lin.comb)
    )
    expect_less_than(
      0,
      nrow(result@ContrastCoefficient)
    )
    expect_less_than(
      0,
      nrow(result@ContrastEstimate)
    )
    expect_less_than(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )
})

expect_error(
  get_result("junk"),
  "'x' is neither an existing file, neither an existing directory"
)
expect_is(
  get_result(temp.dir, datasource.id = this.datasource, n.cluster = 1),
  "n2kResult"
)
expect_is(
  get_result(temp.dir, datasource.id = this.datasource, n.cluster = 2),
  "n2kResult"
)
expect_is(
  get_result(
    temp.dir,
    datasource.id = this.datasource,
    n.cluster = 2 * parallel::detectCores()
  ),
  "n2kResult"
)
expect_message(
  get_result(
    temp.dir,
    datasource.id = this.datasource,
    n.cluster = 2 * parallel::detectCores()
  ),
  paste(
    "Requesting", 2 * parallel::detectCores(), "clusters but only",
    parallel::detectCores(), "available."
  )
)
result <- get_result(
  temp.dir,
  datasource.id = this.datasource,
  keep.fingerprint = TRUE
)
expect_is(result, "n2kResult")
result <- get_result(
  temp.dir,
  datasource.id = this.datasource,
  keep.fingerprint = FALSE
)
expect_is(result, "n2kResult")

# clean temp files
file.remove(
  list.files(
    temp.dir,
    pattern = "^[0-9a-f]{40}\\.rda$",
    full.names = TRUE
  )
)
