test_that("it handles a manifest", {
  project <- "unit_test_fit_model"
  dataset <- test_data()
  object <- n2k_inla(
    result_datasource_id = sha1(sample(letters)),
    scheme_id = sha1(sample(letters)), species_group_id = sha1(sample(letters)),
    location_group_id = sha1(sample(letters)), model_type = "inla poisson: A",
    formula = "Count ~ A", first_imported_year = 1990,
    last_imported_year = 2015, analysis_date = Sys.time(), data = dataset
  )
  object2 <- n2k_inla(
    result_datasource_id = sha1(sample(letters)),
    scheme_id = sha1(sample(letters)), species_group_id = sha1(sample(letters)),
    location_group_id = sha1(sample(letters)), model_type = "inla poisson: B",
    formula = "Count ~ B", first_imported_year = 1990,
    last_imported_year = 2015, analysis_date = Sys.time(), data = dataset
  )
  object3 <- n2k_inla(
    result_datasource_id = sha1(sample(letters)),
    scheme_id = sha1(sample(letters)), species_group_id = sha1(sample(letters)),
    location_group_id = sha1(sample(letters)), model_type = "inla poisson: C",
    formula = "Count ~ C", first_imported_year = 1990,
    last_imported_year = 2015, analysis_date = Sys.time(), data = dataset
  )

  # works with local file
  base <- tempfile("fit_model_manifest")
  dir.create(base)
  store_model(object, base = base, project = project)
  store_model(object2, base = base, project = project)
  store_model(object3, base = base, project = project)
  x <- data.frame(
    fingerprint = c(
      get_file_fingerprint(object), get_file_fingerprint(object2),
      get_file_fingerprint(object3)
    ),
    parent = c(
      NA, get_file_fingerprint(object), get_file_fingerprint(object2)
    ),
    stringsAsFactors = FALSE
  ) %>%
    n2k_manifest()
  expect_invisible(fit_model(x, base = base, project = project))
  x <- store_manifest(x, base, project)
  expect_null(fit_model(x, base = base, project = project))
  expect_null(fit_model(x))

  file.path(base, project) %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()

  # works with an S3 bucket
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  aws_base <- get_bucket(Sys.getenv("N2KBUCKET"))
  store_model(object, base = aws_base, project = project)
  store_model(object2, base = aws_base, project = project)
  store_model(object3, base = aws_base, project = project)
  x <- data.frame(
    fingerprint = c(
      get_file_fingerprint(object), get_file_fingerprint(object2),
      get_file_fingerprint(object3)
    ),
    parent = c(
      NA, get_file_fingerprint(object), get_file_fingerprint(object2)
    ),
    stringsAsFactors = FALSE
  ) %>%
    n2k_manifest()
  expect_invisible(
    fit_model(x, base = aws_base, project = project, verbose = TRUE)
  )

  x <- store_manifest(x, base = aws_base, project = project)
  expect_invisible(fit_model(x$Contents))

  expect_null(fit_model(x$Contents$Key, base = aws_base, project = project))

  available <- get_bucket(aws_base, prefix = project) %>%
    sapply("[[", "Key")
  expect_true(all(sapply(available, delete_object, bucket = aws_base)))
})
