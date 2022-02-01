test_that("n2kimport", {
  status <- "new"
  result_datasource_id <- "result_datasource_id"
  scheme_id <- "scheme_id"
  species_group_id <- "species_group_id"
  location_group_id <- "location_group_id"
  model_type <- "model_type"
  formula <- "count ~ 1"
  first_imported_year <- 1
  last_imported_year <- 10
  analysis_date <- Sys.time()
  dataset <- data.frame(
    filename = "filename", fingerprint = "fingerprint", import_date = Sys.time(),
    stringsAsFactors = TRUE
  )
  expect_is(
    junk <- n2k_import(
      status = status, result_datasource_id = result_datasource_id,
      scheme_id = scheme_id, species_group_id = species_group_id,
      location_group_id = location_group_id, model_type = model_type,
      formula = formula, first_imported_year = first_imported_year,
      last_imported_year = last_imported_year, analysis_date = analysis_date,
      dataset = dataset
    ),
    "n2kImport"
  )
})
