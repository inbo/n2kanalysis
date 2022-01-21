test_that("model imputation works", {
  set.seed(20191213)
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
  dataset <- test_data(missing = 0.2)
  base <- tempdir()
  project <- "imputation"

  imputation <- n2k_inla(
    data = dataset, scheme_id = this_scheme_id,
    result_datasource_id = this_result_datasource_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, imputation_size = 3,
    last_imported_year = this_last_imported_year, family = "poisson",
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    formula = "Count ~ A + f(E, model = \"iid\")",
    analysis_date = Sys.time()
  )
  aggregation <- n2k_aggregate(
    scheme_id = this_scheme_id,
    result_datasource_id = this_result_datasource_id, formula = "~ A + B",
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, analysis_date = Sys.time(),
    last_imported_year = this_last_imported_year, fun = sum,
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    parent = get_file_fingerprint(imputation)
  )
  aggregation2 <- n2k_aggregate(
    scheme_id = this_scheme_id,
    result_datasource_id = this_result_datasource_id, formula = "~ A",
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, analysis_date = Sys.time(),
    last_imported_year = this_last_imported_year, fun = sum,
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    parent = get_file_fingerprint(aggregation)
  )
  extractor <- function(model) {
    model$summary.fixed[, c("mean", "sd")]
  }
  mi <- n2k_model_imputed(
    scheme_id = this_scheme_id, model_args = list(family = "poisson"),
    result_datasource_id = this_result_datasource_id, model_fun = INLA::inla,
    species_group_id = this_species_group_id, extractor = extractor,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, analysis_date = Sys.time(),
    last_imported_year = this_last_imported_year, formula = "~ A",
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    parent = get_file_fingerprint(aggregation)
  )
  pma <- list(
    function(x) {
      return(list(family = "poisson"))
    }
  )
  mi2 <- n2k_model_imputed(
    scheme_id = this_scheme_id, model_args = list(),
    result_datasource_id = this_result_datasource_id, model_fun = INLA::inla,
    species_group_id = this_species_group_id, extractor = extractor,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, analysis_date = Sys.time(),
    last_imported_year = this_last_imported_year, formula = "~ A",
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    parent = get_file_fingerprint(aggregation), prepare_model_args = pma
  )
  store_model(imputation, base, project)
  store_model(aggregation, base, project)
  store_model(mi, base, project)
  store_model(aggregation2, base, project)
  store_model(mi2, base, project)
  expect_message(
    fit_model(
      get_file_fingerprint(imputation), base, project, parallel_configs = FALSE
    ),
    "converged"
  )
  expect_message(
    fit_model(get_file_fingerprint(aggregation), base, project), "converged"
  )
  expect_message(
    fit_model(get_file_fingerprint(mi), base, project), "converged"
  )
  expect_message(
    fit_model(get_file_fingerprint(aggregation2), base, project), "converged"
  )
  expect_message(
    fit_model(get_file_fingerprint(mi2), base, project), "converged"
  )
})
