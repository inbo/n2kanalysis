test_that("it handles a manifest", {
  project <- "unit_test_fit_model"
  dataset <- test_data()
  object <- n2k_inla(
    result_datasource_id = sha1(sample(letters)),
    scheme_id = sha1(sample(letters)),
    species_group_id = sha1(sample(letters)),
    location_group_id = sha1(sample(letters)),
    model_type = "inla poisson: A",
    formula = "Count ~ A",
    first_imported_year = 1990,
    last_imported_year = 2015,
    analysis_date = Sys.time(),
    data = dataset
  )
  object2 <- n2k_inla(
    result_datasource_id = sha1(sample(letters)),
    scheme_id = sha1(sample(letters)),
    species_group_id = sha1(sample(letters)),
    location_group_id = sha1(sample(letters)),
    model_type = "inla poisson: B",
    formula = "Count ~ B",
    first_imported_year = 1990,
    last_imported_year = 2015,
    analysis_date = Sys.time(),
    data = dataset
  )
  object3 <- n2k_inla(
    result_datasource_id = sha1(sample(letters)),
    scheme_id = sha1(sample(letters)),
    species_group_id = sha1(sample(letters)),
    location_group_id = sha1(sample(letters)),
    model_type = "inla poisson: C",
    formula = "Count ~ C",
    first_imported_year = 1990,
    last_imported_year = 2015,
    analysis_date = Sys.time(),
    data = dataset
  )

  # works with local file
  base <- tempfile("fit_model_manifest")
  dir.create(base)
  store_model(object, base = base, project = project)
  store_model(object2, base = base, project = project)
  store_model(object3, base = base, project = project)
  manif <- data.frame(
    fingerprint = c(
      get_file_fingerprint(object),
      get_file_fingerprint(object2),
      get_file_fingerprint(object3)
    ),
    parent = c(
      NA,
      get_file_fingerprint(object),
      get_file_fingerprint(object2)
    ),
    stringsAsFactors = FALSE
  ) |>
    n2k_manifest()
  hash <- store_manifest_yaml(
    x = manif,
    base = base,
    project = project,
    docker = "inbobmk/rn2k:dev-0.10",
    dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@0.4.0")
  )
  script <- manifest_yaml_to_bash(
    base = base,
    project = project,
    hash = basename(hash)
  )
  results <- get_result(
    x = manif,
    base = base,
    project = project,
    verbose = FALSE
  )
  expect_s4_class(results, "n2kResult")
  expect_identical(
    sort(results@AnalysisMetadata$file_fingerprint),
    sort(manif@Manifest$fingerprint)
  )
  expect_true(all(status(results) == "new"))
  expect_invisible(
    fit_model(manif, base = base, project = project, verbose = FALSE)
  )
  y <- store_manifest(manif, base, project)
  expect_null(fit_model(y, base = base, project = project, verbose = FALSE))
  expect_null(fit_model(y, verbose = FALSE))
  results <- get_result(
    x = manif,
    base = base,
    project = project,
    verbose = FALSE
  )
  expect_s4_class(results, "n2kResult")
  expect_identical(
    sort(results@AnalysisMetadata$file_fingerprint),
    sort(manif@Manifest$fingerprint)
  )
  expect_true(all(status(results) == "converged"))
  expect_s4_class(
    results <- get_result(
      x = manif,
      base = base,
      project = project,
      verbose = FALSE
    ),
    "n2kResult"
  )

  file.path(base, project) |>
    list.files(recursive = TRUE, full.names = TRUE) |>
    c(
      R_user_dir("n2kanalysis", which = "cache") |>
        file.path(manif@Fingerprint)
    ) |>
    file.remove()

  # works with an S3 bucket
  if (Sys.getenv("GITHUB_ACTION") == "") {
    connect_inbo_s3()
  }
  aws_base <- get_bucket(Sys.getenv("N2KBUCKET"), max = 1)
  store_model(object, base = aws_base, project = project)
  store_model(object2, base = aws_base, project = project)
  store_model(object3, base = aws_base, project = project)
  manif <- data.frame(
    fingerprint = c(
      get_file_fingerprint(object),
      get_file_fingerprint(object2),
      get_file_fingerprint(object3)
    ),
    parent = c(
      NA,
      get_file_fingerprint(object),
      get_file_fingerprint(object2)
    ),
    stringsAsFactors = FALSE
  ) |>
    n2k_manifest()
  hash <- store_manifest_yaml(
    x = manif,
    base = aws_base,
    project = project,
    docker = "inbobmk/rn2k:dev-0.10",
    dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@0.4.0")
  )
  script <- manifest_yaml_to_bash(
    base = aws_base,
    project = project,
    hash = basename(hash)
  )
  results <- get_result(
    x = manif,
    base = aws_base,
    project = project,
    verbose = FALSE
  )
  expect_s4_class(results, "n2kResult")
  expect_identical(
    sort(results@AnalysisMetadata$file_fingerprint),
    sort(manif@Manifest$fingerprint)
  )
  expect_true(all(status(results) == "new"))
  expect_invisible(
    fit_model(manif, base = aws_base, project = project, verbose = FALSE)
  )
  results <- get_result(
    x = manif,
    base = aws_base,
    project = project,
    verbose = FALSE
  )
  expect_s4_class(results, "n2kResult")
  expect_identical(
    sort(results@AnalysisMetadata$file_fingerprint),
    sort(manif@Manifest$fingerprint)
  )
  expect_true(all(status(results) == "converged"))

  y <- store_manifest(manif, base = aws_base, project = project)
  expect_null(fit_model(y, base = aws_base, project = project, verbose = FALSE))

  available <- get_bucket(aws_base, prefix = project) |>
    sapply("[[", "Key")
  expect_true(all(sapply(available, delete_object, bucket = aws_base)))
  R_user_dir("n2kanalysis", which = "cache") |>
    file.path(manif@Fingerprint) |>
    file.remove()
})
