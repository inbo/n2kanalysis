context("get_result")
temp.dir <- tempdir()
this.result.datasource.id <- sha1(sample(letters))
this.scheme.id <- sha1(sample(letters))
this.species.group.id <- sha1(sample(letters))
this.location.group.id <- sha1(sample(letters))
this.analysis.date <- Sys.time()
this.first.imported.year <- 1990L
this.last.imported.year <- 2015L
this.last.analysed.year <- 2014L
this.duration <- 1L
this.datasource <- sha1(letters)
dataset <- test_data(this.datasource)
describe("get_result on n2kInla", {
  this.model.type <- "inla nbinomial: period + herd"
  this.formula <-
    "Count ~
      A * (B + C) + C:D +
      f(E, model = 'rw1', replicate = as.integer(A)) +
      f(F, model = 'iid')"
  analysis <- n2k_inla(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    family = "nbinomial",
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset
  )
  result <- get_result(
    analysis,
    datasource.id = this.datasource,
    verbose = FALSE
  )
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
  filename <- store_model(analysis, base = temp.dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename),
    datasource.id = this.datasource,
    verbose = FALSE
  )
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_lt(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_lt(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )

  # with linear combination
  lin.comb <- dataset %>%
    filter(
      .data$C == max(.data$C),
      .data$D == max(.data$D)
    ) %>%
    select("A", "B", "C", "D") %>%
    distinct_() %>%
    model.matrix(object = ~A * (B + C) + C:D)
  rownames(lin.comb) <- seq_len(nrow(lin.comb))
  this.parent <- "abcd"
  analysis <- n2k_inla(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    family = "nbinomial",
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = lin.comb,
    parent = this.parent
  )
  result <- get_result(
    analysis,
    datasource.id = this.datasource,
    verbose = FALSE
  )
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
  filename <- store_model(analysis, base = temp.dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename),
    datasource.id = this.datasource,
    verbose = FALSE
  )
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_lt(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      nrow(lin.comb)
    )
    expect_lt(
      0,
      nrow(result@ContrastCoefficient)
    )
    expect_lt(
      0,
      nrow(result@ContrastEstimate)
    )
    expect_lt(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )

  # with linear combination as list of vectors
  lin.comb <- as.list(as.data.frame(lin.comb))
  names(lin.comb[[1]]) <- seq_along(lin.comb[[1]])
  analysis <- n2k_inla(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    family = "nbinomial",
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = lin.comb,
    parent = this.parent
  )
  result <- get_result(
    analysis,
    datasource.id = this.datasource,
    verbose = FALSE
  )
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
      length(lin.comb[[1]])
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
  filename <- store_model(analysis, base = temp.dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename),
    datasource.id = this.datasource,
    verbose = FALSE
  )
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_lt(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      length(lin.comb[[1]])
    )
    expect_lt(
      0,
      nrow(result@ContrastCoefficient)
    )
    expect_lt(
      0,
      nrow(result@ContrastEstimate)
    )
    expect_lt(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )


  # with linear combination as list of matrices
  lc.E <- max(dataset$E) %>%
    diag() %>%
    list() %>%
    rep(length(levels(dataset$A))) %>%
    do.call(what = cbind) %>%
    "/"(length(levels(dataset$A))) #nolint
  colnames(lc.E) <- dataset %>%
    select("A", "E") %>%
    distinct_() %>%
    arrange_(~A, ~E) %>%
    transmute_(~paste(A, E, sep = ":")) %>%
    unlist() %>%
    unname()
  rownames(lc.E) <- seq_len(nrow(lc.E))
  lin.comb <- list(
    E = lc.E,
    F = matrix(c(1, 0, 0), byrow = TRUE, ncol = 3, nrow = nrow(lc.E))
  )
  analysis <- n2k_inla(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    family = "nbinomial",
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset,
    lin.comb = lin.comb,
    replicate.name = list(
      E = levels(dataset$A)
    ),
    parent = this.parent
  )
  result <- get_result(
    analysis,
    datasource.id = this.datasource,
    verbose = FALSE
  )
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
      nrow(lin.comb[[1]])
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
  filename <- store_model(analysis, base = temp.dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename),
    datasource.id = this.datasource,
    verbose = FALSE
  )
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_lt(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      nrow(lin.comb[[1]])
    )
    expect_lt(
      0,
      nrow(result@ContrastCoefficient)
    )
    expect_lt(
      0,
      nrow(result@ContrastEstimate)
    )
    expect_lt(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
})

expect_error(
  get_result("junk"),
  "'x' is neither an existing file, neither an existing directory"
)
expect_is(
  get_result(
    temp.dir,
    datasource.id = this.datasource,
    n.cluster = 1,
    verbose = FALSE
  ),
  "n2kResult"
)
# nolint start
# expect_is(
#   get_result(temp.dir, datasource.id = this.datasource, n.cluster = 2),
#   "n2kResult"
# )
# expect_is(
#   get_result(
#     temp.dir,
#     datasource.id = this.datasource,
#     n.cluster = 2 * parallel::detectCores()
#   ),
#   "n2kResult"
# )
# expect_message(
#   get_result(
#     temp.dir,
#     datasource.id = this.datasource,
#     n.cluster = 2 * parallel::detectCores()
#   ),
#   paste(
#     "Requesting", 2 * parallel::detectCores(), "clusters but only",
#     parallel::detectCores(), "available."
#   )
# )
# nolint end

data("cake", package = "lme4")
cake$ObservationID <- seq_len(nrow(cake))
cake$DataFieldID <- this.datasource
describe("get_result on n2kInla with replicated random effects", {
  this.model.type <- "inla nbinomial: recipe + replicate + temperature"
  this.formula <-
"angle ~ recipe + f(replicate, model = \"iid\") +
  f(as.integer(temperature), model = \"rw1\", replicate = as.integer(recipe))"
  analysis <- n2k_inla(
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    family = "nbinomial",
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cake
  )
  result <- get_result(
    analysis,
    datasource.id = this.datasource,
    verbose = FALSE
  )
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
  filename <- store_model(analysis, base = temp.dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename),
    datasource.id = this.datasource,
    verbose = FALSE
  )
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_lt(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_lt(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
})

describe("get_result on n2kGlmerPoisson", {
  this.model.type <- "glmer poisson: period + herd"
  this.formula <- "Count ~ A * (B + C) + C:D + (1|E) + (1|F)"
  analysis <- n2k_glmer_poisson(
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
  result <- get_result(
    analysis,
    datasource.id = this.datasource,
    verbose = FALSE
  )
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
  filename <- store_model(analysis, base = temp.dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename),
    datasource.id = this.datasource,
    verbose = FALSE
  )
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_lt(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_lt(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource, verbose = FALSE),
    result
  )
})

# clean temp files
file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
