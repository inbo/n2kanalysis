context("fit_model")
describe("fit_model() on GlmerPoisson based objects", {
  temp_dir <- tempdir()
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DataFieldID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this_analysis_date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this_seed <- 1L
  object <- n2k_glmer_poisson(
    result_datasource_id = sha1(letters),
    scheme_id = sha1(letters),
    species_group_id = sha1(letters),
    location_group_id = sha1(letters),
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first_imported_year = 1990L,
    last_imported_year = 2015L,
    analysis_date = this_analysis_date,
    seed = this_seed,
    data = cbpp
  )
  weighted.object <- n2k_glmer_poisson(
    result_datasource_id = sha1(letters),
    scheme_id = sha1(letters),
    species_group_id = sha1(letters),
    location_group_id = sha1(letters),
    model.type = "weighted glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first_imported_year = 1990L,
    last_imported_year = 2015L,
    analysis_date = this_analysis_date,
    seed = this_seed,
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
  object.file <- "64b56280b79201c5151dd3cb165b2fee9bf6de36"
  weighted.object.file <- "0e36a4dc07d236286c8c6a679aff34a74ce22190"

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
    expect_true(validObject(object.fit))
    expect_true(validObject(weighted.object.fit))
  })
  it("works with objects saved in rds files", {
    filename <- store_model(object, base = temp_dir, project = "fit_model")
    expect_identical(status(filename)$Status, "new")
    fit_model(filename, verbose = FALSE)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
    filename <- store_model(
      weighted.object,
      base = temp_dir,
      project = "fit_model"
    )
    expect_identical(status(filename)$Status, "new")
    fit_model(filename, verbose = FALSE)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
  })

  # clean temp files
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE))
})

describe("fit_model() on INLA based objects", {
  temp_dir <- tempdir()
  dataset <- test_data(missing = 0.2)
  this_analysis_date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_model.type <- "inla poisson: A + B + C + D"
  this_formula <-
    "Count ~ A * (B + C) + C * D + f(E, model = 'iid')"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last.analysed_year <- 2014L
  this_duration <- 1L
  lin.comb <- dataset %>%
    distinct(.data$A) %>%
    model.matrix(object = ~A)
  rownames(lin.comb) <- seq_len(nrow(lin.comb))
  bad.lin.comb <- lin.comb[, -1]
  lin.comb.list <- as.list(as.data.frame(lin.comb))
  names(lin.comb.list[[1]]) <- seq_along(lin.comb.list[[1]])
  lin.comb.list2 <- list(E = diag(length(unique(dataset$E))))
  rownames(lin.comb.list2[[1]]) <- seq_along(unique(dataset$E))
  object <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset
  )
  object.lc <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset,
    lin.comb = lin.comb
  )
  object.lc.list <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset,
    lin.comb = lin.comb.list
  )
  object.lc.list2 <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset,
    lin.comb = lin.comb.list2
  )
  object.badlc <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset,
    lin.comb = bad.lin.comb
  )
  object.imp <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
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
  object.file <- "b662c99aae23839f2754da0debd1b44184fd6ba9"
  object.lc.file <- "f6efab494ad42de4effbe59a2a9357bbd7894647"
  object.lc.list.file <- "836ce2546888bf1d258753d454b05b89cbdc6e87"
  object.lc.list2.file <- "8efa2b8081c3e785a945e1fcab70550306dfa8dc"
  object.badlc.file <- "cc06786df1e1a6490c4ba6ba55e5b26415e1d556"
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
    expect_true(validObject(object.fit))
    expect_true(validObject(object.lc.fit))
  })
  it("works with objects saved in rds files", {
    analysis <- object
    filename <- store_model(analysis, base = temp_dir, project = "fit_model")
    expect_identical(status(filename)$Status, "new")
    fit_model(filename)
    filename <- gsub("new", "converged", filename)
    expect_identical(
      status(filename)$Status,
      "converged"
    )
    analysis <- object.lc
    filename <- store_model(analysis, base = temp_dir, project = "fit_model")
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
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE))
})

test_that("fit_model() works on n2kInlaComparison", {
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_analysis_date <- Sys.time()
  this_model.type <- "inla poisson: A * (B + C) + C:D"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last.analysed_year <- 2014L
  this_duration <- 1L
  dataset <- test_data()
  temp_dir <- tempdir()

  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = "Count ~ A",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset
  )
  p1 <- get_file_fingerprint(analysis)
  filename1 <- store_model(analysis, base = temp_dir, project = "fit_model")
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = "Count ~ A * B",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset
  )
  p2 <- get_file_fingerprint(analysis)
  filename2 <- store_model(analysis, base = temp_dir, project = "fit_model")

  analysis <- n2k_inla_comparison(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    formula = "~B", #nolint
    model.type = "inla comparison: A*B",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    parent_status = status(temp_dir) %>%
      select(
        ParentAnalysis = "FileFingerprint",
        ParentStatus = "Status",
        ParentStatusFingerprint = "StatusFingerprint"
      )
  )
  filename3 <- store_model(analysis, base = temp_dir, project = "fit_model")
  fit_model(
    get_file_fingerprint(analysis),
    base = temp_dir,
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
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE))
})

test_that("fit_model() works on n2kInlaComposite", {
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_analysis_date <- Sys.time()
  this_model.type <- "inla poisson: A * (B + C) + C:D"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last.analysed_year <- 2014L
  this_duration <- 1L
  dataset <- test_data()
  temp_dir <- tempdir()

  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = "Count ~ A",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset
  )
  p1 <- get_file_fingerprint(analysis)
  filename1 <- store_model(analysis, base = temp_dir, project = "fit_model")
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model.type = this_model.type,
    formula = "Count ~ A + B",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset
  )
  p2 <- get_file_fingerprint(analysis)
  filename2 <- store_model(analysis, base = temp_dir, project = "fit_model")

  analysis <- n2k_composite(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    formula = "~B", #nolint
    model.type = "inla comparison: A*B",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    parent_status = status(temp_dir) %>%
      select(
        ParentAnalysis = "FileFingerprint",
        ParentStatus = "Status",
        ParentStatusFingerprint = "StatusFingerprint"
      ),
    extractor = function(model) {
      relevant <- grep("^A", rownames(model$summary.fixed))
      model$summary.fixed[relevant, c("mean", "sd")] %>%
        rownames_to_column("Value") %>%
        transmute(
          .data$Value,
          Estimate = .data$mean,
          Variance = .data$sd ^ 2)
    }
  )

  filename3 <- store_model(analysis, base = temp_dir, project = "fit_model")
  fit_model(
    get_file_fingerprint(analysis),
    base = temp_dir,
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
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE))
})
