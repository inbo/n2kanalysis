test_that("fit_model() on INLA based objects", {
  temp_dir <- tempfile("fit_inla")
  dir.create(temp_dir)
  dataset <- test_data(missing = 0.2)
  this_analysis_date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_model_type <- "inla poisson: A + B + C + D"
  this_formula <- "Count ~ A * (B + C) + C * D + f(E, model = 'iid')"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2014L
  this_duration <- 1L
  lin_comb <- dataset %>%
    distinct(.data$A) %>%
    model.matrix(object = ~A)
  rownames(lin_comb) <- seq_len(nrow(lin_comb))
  bad_lin_comb <- lin_comb[, -1]
  lin_comb_list <- as.list(as.data.frame(lin_comb))
  names(lin_comb_list[[1]]) <- seq_along(lin_comb_list[[1]])
  lin_comb_list2 <- list(E = diag(length(unique(dataset$E))))
  rownames(lin_comb_list2[[1]]) <- seq_along(unique(dataset$E))
  object <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  object_lc <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset, lin_comb = lin_comb
  )
  object_lc_list <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset, lin_comb = lin_comb_list
  )
  object_lc_list2 <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset,
    lin_comb = lin_comb_list2
  )
  object_badlc <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset, lin_comb = bad_lin_comb
  )
  object_imp <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, imputation.size = 10, data = dataset
  )
  object_fit <- fit_model(object)
  object_lc_fit <- fit_model(object_lc)
  object_lc_list_fit <- fit_model(object_lc_list)
  object_lc_list2_fit <- fit_model(object_lc_list2)
  object_badlc_fit <- fit_model(object_badlc)
  object_imp_fit <- fit_model(object_imp)
  cat(
    "\nobject_file <- \"", get_file_fingerprint(object), "\"\n",
    "object_lc_file <- \"", get_file_fingerprint(object_lc), "\"\n",
    "object_lc_list_file <- \"", get_file_fingerprint(object_lc_list), "\"\n",
    "object_lc_list2_file <- \"", get_file_fingerprint(object_lc_list2), "\"\n",
    "object_badlc_file <- \"", get_file_fingerprint(object_badlc), "\"\n",
    sep = ""
  )
  # 64-bit linux
  object_file <- "f4f6b81193924031699ea89948626f20a573d18d"
  object_lc_file <- "6fcd90fbd66a5cd5a523bba0d30bc44c3ac7b66b"
  object_lc_list_file <- "1b4fd0e42aa8564853af9a78f32baf343092924f"
  object_lc_list2_file <- "f207d11bf9c278e952495b1a640f50e383bc0ed7"
  object_badlc_file <- "448d9542be0742b236594645dcc5f4bff0e41403"

  # returns the same file fingerprints on 32-bit and 64-bit
  expect_identical(object_file, get_file_fingerprint(object))
  expect_identical(object_lc_file, get_file_fingerprint(object_lc))
  expect_identical(object_lc_list_file, get_file_fingerprint(object_lc_list))
  expect_identical(
    object_lc_list2_file, get_file_fingerprint(object_lc_list2)
  )
  expect_identical(object_badlc_file, get_file_fingerprint(object_badlc))
  # doesn't alter the file fingerprint when fitting a model
  expect_identical(
    get_file_fingerprint(object), get_file_fingerprint(object_fit)
  )
  expect_identical(
    get_file_fingerprint(object_lc), get_file_fingerprint(object_lc_fit)
  )

  # returns valid objects
  expect_true(validObject(object_fit))
  expect_true(validObject(object_lc_fit))

  # works with objects saved in rds files
  analysis <- object
  filename <- store_model(analysis, base = temp_dir, project = "fit_model")
  expect_identical(status(filename)$status, "new")
  suppressWarnings(suppressMessages(fit_model(filename)))
  filename <- list.files(
    temp_dir, pattern = basename(filename), recursive = TRUE, full.names = TRUE
  )
  expect_identical(status(filename)$status, "converged")
  analysis <- object_lc
  filename <- store_model(analysis, base = temp_dir, project = "fit_model")
  expect_identical(status(filename)$status, "new")
  suppressWarnings(suppressMessages(fit_model(filename)))
  filename <- list.files(
    temp_dir, pattern = basename(filename), recursive = TRUE, full.names = TRUE
  )
  expect_identical(status(filename)$status, "converged")

  # doesn't refit converged models with the default status
  expect_identical(fit_model(object_fit), object_fit)
  expect_identical(fit_model(object_lc_fit), object_lc_fit)

  # returns an error when the linear combination is not valid
  expect_identical(status(object_badlc_fit), "error")

  # test time-out
  skip_on_os("windows")
  object_long <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, lin_comb = lin_comb,
    data = list(dataset) |>
      rep(10) |>
      bind_rows() |>
      mutate(observation_id = seq_along(.data$observation_id))
  )
  timeout_object <- fit_model(object_long, timeout = 1e-3)
  expect_identical(status(timeout_object), "time-out")
})

test_that("fit_model() works on n2kInlaComparison", {
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_analysis_date <- Sys.time()
  this_model_type <- "inla poisson: A * (B + C) + C:D"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2014L
  this_duration <- 1L
  dataset <- test_data()
  temp_dir <- tempfile("fit_inla_comparison")
  dir.create(temp_dir)

  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = "Count ~ A", first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  p1 <- get_file_fingerprint(analysis)
  filename1 <- store_model(analysis, base = temp_dir, project = "fit_model")
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = "Count ~ A * B", first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  p2 <- get_file_fingerprint(analysis)
  filename2 <- store_model(analysis, base = temp_dir, project = "fit_model")

  analysis <- n2k_inla_comparison(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, formula = "~B",
    model_type = "inla comparison: A*B",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    parent_status = status(temp_dir) %>%
      select(
        parent_analysis = "file_fingerprint",
        parent_status = "status",
        parentstatus_fingerprint = "status_fingerprint"
      )
  )
  filename3 <- store_model(analysis, base = temp_dir, project = "fit_model")
  fit_model(
    get_file_fingerprint(analysis), base = temp_dir, project = "fit_model",
    verbose = FALSE
  )

  fit_model(filename1, verbose = FALSE)
  fit_model(filename3, verbose = FALSE)

  fit_model(filename2, verbose = FALSE)
  fit_model(filename3, verbose = FALSE)
  filename3 <- gsub("waiting", "converged", filename3)
  expect_null(fit_model(filename3, verbose = FALSE))
})

test_that("fit_model() works on n2kInlaComposite", {
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_analysis_date <- Sys.time()
  this_model_type <- "inla poisson: A * (B + C) + C:D"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2014L
  this_duration <- 1L
  dataset <- test_data()
  temp_dir <- tempfile("fit_inla_composite")
  dir.create(temp_dir)

  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = "Count ~ A", first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  p1 <- get_file_fingerprint(analysis)
  filename1 <- store_model(analysis, base = temp_dir, project = "fit_model")
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = "Count ~ A + B", first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  p2 <- get_file_fingerprint(analysis)
  filename2 <- store_model(analysis, base = temp_dir, project = "fit_model")

  analysis <- n2k_composite(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, formula = "~B",
    model_type = "inla comparison: A*B",
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    parent_status = status(temp_dir) %>%
      select(
        parent_analysis = "file_fingerprint",
        parent_status = "status",
        parentstatus_fingerprint = "status_fingerprint"
      ),
    extractor = function(model) {
      relevant <- grep("^A", rownames(model$summary.fixed))
      model$summary.fixed[relevant, c("mean", "sd")] %>%
        rownames_to_column("value") %>%
        transmute(
          .data$value,
          estimate = .data$mean,
          variance = .data$sd ^ 2)
    }
  )

  filename3 <- store_model(analysis, base = temp_dir, project = "fit_model")
  fit_model(
    get_file_fingerprint(analysis), base = temp_dir, project = "fit_model",
    verbose = FALSE
  )

  fit_model(filename1, verbose = FALSE)
  fit_model(filename3, verbose = FALSE)

  fit_model(filename2, verbose = FALSE)
  fit_model(filename3, verbose = FALSE)
  filename3 <- gsub("waiting", "converged", filename3)
  expect_null(fit_model(filename3, verbose = FALSE))
})

test_that("fit_model() works on n2kHurdleImputed", {
  project <- "fit_inla_hurdle"
  base <- tempfile(project)
  dir.create(base)
  dataset <- test_data(missing = 0.2)
  this_date <- Sys.time() - 24 * 3600
  dataset |>
    transmute(
      .data$A, Count = ifelse(.data$Count > 0, .data$Count, NA),
      .data$observation_id, .data$datafield_id
    ) |>
    n2k_inla(
      result_datasource_id = "a", scheme_id = "b", seed = 20230922,
      model_type = "inla zeroinflatednbinomial0: A",
      family = "zeroinflatednbinomial0", formula = "Count ~ 1",
      species_group_id = "c", location_group_id = "d", first_imported_year = 1,
      last_imported_year = 10, imputation_size = 9, analysis_date = this_date,
      control = list(
        control.family = list(
          list(hyper = list(theta = list(initial = -11, fixed = TRUE)))
        )
      )
    ) -> count
  dataset |>
    transmute(
      .data$A, Presence = ifelse(.data$Count > 0, 1, 0),
      .data$observation_id, .data$datafield_id
    ) |>
    n2k_inla(
      result_datasource_id = "a", scheme_id = "b",
      model_type = "inla binomial: A",
      family = "binomial", formula = "Presence ~ 1", seed = 20230922,
      species_group_id = "c", location_group_id = "d", first_imported_year = 1,
      last_imported_year = 10, imputation_size = 9, analysis_date = this_date
    ) -> presence
  hurdle <- n2k_hurdle_imputed(presence = presence, count = count)
  expect_s4_class(fit_model(hurdle, status = "error"), "n2kHurdleImputed")
  sha_count <- store_model(count, base = base, project = project)
  sha_presence <- store_model(presence, base = base, project = project)
  sha_hurdle <- store_model(hurdle, base = base, project = project)
  expect_null(fit_model(basename(sha_hurdle), base = base, project = project))
  suppressWarnings(
    expect_null(fit_model(basename(sha_count), base = base, project = project))
  )
  suppressWarnings(
    expect_null(
      fit_model(basename(sha_presence), base = base, project = project)
    )
  )
  suppressWarnings(
    expect_null(fit_model(basename(sha_hurdle), base = base, project = project))
  )
  expect_s4_class(
    basename(sha_hurdle) |>
      read_model(base = base, project = project) |>
      get_anomaly(),
    "n2kAnomaly"
  )
})
