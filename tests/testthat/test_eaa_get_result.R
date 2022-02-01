test_that("get_result on n2kInla", {
  expect_error(
    get_result("junk"),
    "'x' is neither an existing file, neither an existing directory"
  )

  temp_dir <- tempdir()
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_analysis_date <- Sys.time()
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2014L
  this_duration <- 1L
  this_datasource <- sha1(letters)
  dataset <- test_data(this_datasource)

  this_model_type <- "inla nbinomial: A * B + E"
  this_formula <-
    "Count ~
      A * (B + C) + C:D +
      f(E, model = 'rw1', replicate = as.integer(A)) +
      f(G, model = 'iid')"
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, family = "nbinomial",
    model_type = this_model_type, formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  result <- get_result(
    analysis, datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_identical(nrow(result@Parameter), 0L)
  expect_identical(nrow(result@Contrast), 0L)
  expect_identical(nrow(result@Anomaly), 0L)

  filename <- store_model(analysis, base = temp_dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename), datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_lt(0, nrow(result@Parameter))
  expect_identical(nrow(result@Contrast), 0L)
  expect_lt(0, nrow(result@Anomaly))
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )

  # with linear combination
  lin_comb <- dataset %>%
    filter(.data$C == max(.data$C), .data$D == max(.data$D)) %>%
    select("A", "B", "C", "D") %>%
    distinct() %>%
    model.matrix(object = ~A * (B + C) + C:D)
  rownames(lin_comb) <- seq_len(nrow(lin_comb))
  this_parent <- "abcd"
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, family = "nbinomial",
    model_type = this_model_type, formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset, lin_comb = lin_comb,
    parent = this_parent
  )
  result <- get_result(
    analysis, datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_identical(nrow(result@Parameter), 0L)
  expect_identical(nrow(result@Contrast), nrow(lin_comb))
  expect_identical(nrow(result@ContrastCoefficient), 0L)
  expect_identical(nrow(result@ContrastEstimate), 0L)
  expect_identical(nrow(result@Anomaly), 0L)

  filename <- store_model(analysis, base = temp_dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename), datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_lt(0, nrow(result@Parameter))
  expect_identical(nrow(result@Contrast), nrow(lin_comb))
  expect_lt(0, nrow(result@ContrastCoefficient))
  expect_lt(0, nrow(result@ContrastEstimate))
  expect_lt(0, nrow(result@Anomaly))
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )

  # with linear combination as list of vectors
  lin_comb <- as.list(as.data.frame(lin_comb))
  names(lin_comb[[1]]) <- seq_along(lin_comb[[1]])
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, family = "nbinomial",
    model_type = this_model_type, formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset, lin_comb = lin_comb,
    parent = this_parent
  )
  result <- get_result(
    analysis, datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_identical(nrow(result@Parameter), 0L)
  expect_identical(nrow(result@Contrast), length(lin_comb[[1]]))
  expect_identical(nrow(result@ContrastCoefficient), 0L)
  expect_identical(nrow(result@ContrastEstimate), 0L)
  expect_identical(nrow(result@Anomaly), 0L)
  filename <- store_model(analysis, base = temp_dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename), datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_lt(0, nrow(result@Parameter))
  expect_identical(nrow(result@Contrast), length(lin_comb[[1]]))
  expect_lt(0, nrow(result@ContrastCoefficient))
  expect_lt(0, nrow(result@ContrastEstimate))
  expect_lt(0, nrow(result@Anomaly))
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )


  # with linear combination as list of matrices
  lc_e <- max(dataset$E) %>%
    diag() %>%
    list() %>%
    rep(length(levels(dataset$A))) %>%
    do.call(what = cbind) %>%
    "/"(length(levels(dataset$A))) #nolint
  colnames(lc_e) <- dataset %>%
    select("A", "E") %>%
    distinct() %>%
    arrange(.data$A, .data$E) %>%
    transmute(paste(.data$A, .data$E, sep = ":")) %>%
    unlist() %>%
    unname()
  rownames(lc_e) <- seq_len(nrow(lc_e))
  lin_comb <- list(
    E = lc_e,
    G = matrix(c(1, 0, 0), byrow = TRUE, ncol = 3, nrow = nrow(lc_e))
  )
  analysis <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    family = "nbinomial", formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset, lin_comb = lin_comb,
    replicate_name = list(E = levels(dataset$A)),
    parent = this_parent
  )
  result <- get_result(
    analysis, datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_identical(nrow(result@Parameter), 0L)
  expect_identical(nrow(result@Contrast), nrow(lin_comb[[1]]))
  expect_identical(nrow(result@ContrastCoefficient), 0L)
  expect_identical(nrow(result@ContrastEstimate), 0L)
  expect_identical(nrow(result@Anomaly), 0L)
  filename <- store_model(analysis, base = temp_dir, project = "get_result")
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )
  fit_model(filename, verbose = FALSE)
  filename <- gsub(pattern = "new", replacement = "converged", filename)
  result <- get_result(
    readRDS(filename), datasource_id = this_datasource, verbose = FALSE
  )
  expect_is(result, "n2kResult")
  expect_lt(0, nrow(result@Parameter))
  expect_identical(nrow(result@Contrast), nrow(lin_comb[[1]]))
  expect_lt(0, nrow(result@ContrastCoefficient))
  expect_lt(0, nrow(result@ContrastEstimate))
  expect_lt(0, nrow(result@Anomaly))
  expect_equal(
    get_result(filename, datasource_id = this_datasource, verbose = FALSE),
    result
  )
  # clean temp files
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE))
})
