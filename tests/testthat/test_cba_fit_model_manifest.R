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
  ) |>
    n2k_manifest()
  hash <- store_manifest_yaml(
    x = x, base = base, project = project, docker = "inbobmk/rn2k:dev-0.10",
    dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@0.4.0")
  )
  script <- manifest_yaml_to_bash(
    base = base, project = project, hash = basename(hash)
  )
  expect_invisible(fit_model(x, base = base, project = project))
  y <- store_manifest(x, base, project)
  expect_null(fit_model(y, base = base, project = project))
  expect_null(fit_model(y))

  file.path(base, project) |>
    list.files(recursive = TRUE, full.names = TRUE) |>
    c(
      R_user_dir("n2kanalysis", which = "cache") |>
        file.path(x@Fingerprint)
    ) |>
    file.remove()


  # works with an S3 bucket
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  aws_base <- get_bucket(Sys.getenv("N2KBUCKET"), max = 1)
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
  ) |>
    n2k_manifest()
  hash <- store_manifest_yaml(
    x = x, base = aws_base, project = project, docker = "inbobmk/rn2k:dev-0.10",
    dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@0.4.0")
  )
  script <- manifest_yaml_to_bash(
    base = aws_base, project = project, hash = basename(hash)
  )
  expect_s3_class(
    results <- get_result(x, base = aws_base, project = project),
    "data.frame"
  )
  expect_true(all(results$status == "new"))
  expect_invisible(
    fit_model(x, base = aws_base, project = project, verbose = TRUE)
  )
  expect_s3_class(
    results <- get_result(x, base = aws_base, project = project),
    "data.frame"
  )
  expect_true(all(results$status == "converged"))

  y <- store_manifest(x, base = aws_base, project = project)
  expect_invisible(fit_model(y$Contents))

  expect_null(fit_model(y$Contents$Key, base = aws_base, project = project))

  available <- get_bucket(aws_base, prefix = project) %>%
    sapply("[[", "Key")
  expect_true(all(sapply(available, delete_object, bucket = aws_base)))
  R_user_dir("n2kanalysis", which = "cache") |>
    file.path(x@Fingerprint) |>
    file.remove()
})
