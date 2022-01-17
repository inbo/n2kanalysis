test_that("prepare a n2kAggregate object", {
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_analysis_date <- Sys.time()
  this_model_type <- "inla poisson: A * (B + C) + C:D"
  this_formula <-
    "Count ~ A * (B + C) + C:D +
      f(E, model = \"rw1\", replicate = as.integer(A)) +
      f(G, model = \"iid\")"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2014L
  this_duration <- 1L
  dataset <- test_data()
  object <- n2k_inla(
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    model_type = this_model_type,
    formula = this_formula,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date,
    data = dataset
  )
  object <- fit_model(object)
  expect_is(
    child <- n2k_aggregate(
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      parent = get_file_fingerprint(object),
      fun = sum
    ),
    "n2kAggregate"
  )
  base <- tempdir()
  project <- "n2kaggregate"
  expect_is(store_model(object, base = base, project = project), "character")
  expect_is(
    parent <- get_parents(child, base = base, project = project), "list"
  )
  expect_setequal(class(parent[[1]]), "n2kInla")

  # clean temp files
  file.remove(list.files(base, recursive = TRUE, full.names = TRUE))
})
