context("store_manifest_yaml")
test_that("store_manifest_yaml stores the manifest on an S3 bucket", {
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  bucket <- get_bucket("n2kmonitoring")
  object <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  project <- "unittest_store_manifest_yaml"
  docker <- "inbobmk/rn2k:latest"
  dependencies <- c("inbo/n2khelper@v0.4.1", "inbo/n2kanalysis@docker") # nolint: nonportable_path_linter, line_length_linter.
  expect_is(
    stored <- store_manifest_yaml(
      x = object, base = bucket, project = project, docker = docker,
      dependencies = dependencies
    ),
    "s3_bucket"
  )
  available <- get_bucket(bucket, prefix = project)
  keys <- sapply(
    available,
    function(x) {
      x$Key
    }
  )
  expect_equivalent(stored, available[grepl("\\.yaml$", keys)])
  expect_equivalent(object, read_manifest(bucket, project, object@Fingerprint))

  expect_is(
    stored2 <- store_manifest_yaml(
      x = object, base = bucket, project = project, docker = docker,
      dependencies = dependencies
    ),
    "s3_bucket"
  )
  available <- get_bucket(bucket, prefix = "unittest_store_manifest")
  keys <- sapply(
    available,
    function(x) {
      x$Key
    }
  )
  expect_equivalent(stored2, available[grepl("\\.yaml$", keys)])
  expect_equivalent(object, read_manifest(bucket, project, object@Fingerprint))
  expect_equivalent(stored, stored2)
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})

test_that("store_manifest_yaml stores the manifest on a file system", {
  base <- tempdir()
  object <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  project <- "unittest_store_manifest_yaml"
  docker <- "inbobmk/rn2k:latest"
  dependencies <- c("inbo/n2khelper@v0.4.1", "inbo/n2kanalysis@docker") # nolint: nonportable_path_linter, line_length_linter.
  expect_is(
    stored <- store_manifest_yaml(
      x = object, base = base, project = project, docker = docker,
      dependencies = dependencies
    ),
    "character"
  )
  expect_true(file_test("-f", stored))
  expect_equivalent(object, read_manifest(base, project, object@Fingerprint))

  expect_is(
    stored2 <- store_manifest_yaml(
      x = object, base = base, project = project, docker = docker,
      dependencies = dependencies
    ),
    "character"
  )
  expect_identical(stored2, stored)
  file.remove(stored)
})
