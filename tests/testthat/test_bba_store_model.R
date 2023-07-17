test_that("store_model stores the model on a local file system", {
  base <- tempfile("store_model")
  dir.create(base)
  project <- "store_model"
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
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  expect_is(filename <- store_model(object, base, project), "character")
  file_info <- file.info(filename)
  expect_is(
    filename2 <- store_model(object, base, project, overwrite = FALSE),
    "character"
  )
  file_info2 <- file.info(filename2)
  expect_equal(filename, filename2)
  expect_equal(file_info, file_info2, check.attributes = FALSE)

  expect_identical(get_model(filename), NULL)

  fitted <- fit_model(filename, base, project)
  expect_equal(
    file.path(base, project) %>%
      list.files(recursive = TRUE, full.names = TRUE) %>%
      normalizePath(winslash = "/", mustWork = TRUE),
    as.character(filename) |>
      gsub(pattern = "new", replacement = "converged") |>
      normalizePath(winslash = "/", mustWork = TRUE)
  )
  expect_is(
    filename2 <- store_model(object, base, project, overwrite = FALSE),
    "character"
  )
  expect_equal(filename2, gsub("new", "converged", filename))
  expect_is(filename2 <- store_model(object, base, project), "character")
  expect_equal(filename2, filename)

  file.path(base, project) %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("store_model stores the model on an S3 bucket", {
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  bucket <- get_bucket(Sys.getenv("N2KBUCKET"))
  project <- "unittest_store_model"
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
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    formula = this_formula, first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    analysis_date = this_analysis_date, data = dataset
  )
  expect_is(
    filename <- store_model(x = object, base = bucket, project = project),
    "character"
  )
  available <- get_bucket(Sys.getenv("N2KBUCKET"), prefix = project) %>%
    sapply("[[", "Key")
  expect_true(filename %in% available)
  expect_is(
    filename2 <- store_model(x = object, base = bucket, project = project),
    "character"
  )
  available <- get_bucket(Sys.getenv("N2KBUCKET"), prefix = project) %>%
    sapply("[[", "Key")
  expect_true(filename2 %in% available)
  expect_identical(filename, filename2)
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
