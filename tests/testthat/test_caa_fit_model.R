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
  expect_identical(status(object_lc_list2), "new")
  timeout_object <- fit_model(object_lc_list2, timeout = 0.0001)
  expect_identical(status(timeout_object), "time-out")
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
  object_file <- "2774e9be02ef2fabef43967c36626891cdfd8d56"
  object_lc_file <- "5d0c64555c3588002ddb526918ed4a21882c9882"
  object_lc_list_file <- "8631c63afc8c32d2bf4363497144180a97cad685"
  object_lc_list2_file <- "342daede65a8817ff76ddb76cd2efb1813c667eb"
  object_badlc_file <- "6e33396f9013b908b774ff7a11adc55b4eb293b9"

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
