context("store_manifest_yaml")
test_that("store_manifest_yaml stores the manifest on an S3 bucket", {
  bucket <- get_bucket("n2kmonitoring")
  object <- n2k_manifest(
    data.frame(
      Fingerprint = "1",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  project <- "unittest_store_manifest_yaml"
  docker <- "inbobmk/rn2k:latest"
  dependencies <- c("inbo/n2khelper@v0.4.1", "inbo/n2kanalysis@docker")
  expect_is(
    stored <- store_manifest_yaml(
      x = object,
      base = bucket,
      project = project,
      docker = docker,
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
  expect_equivalent(
    stored,
    available[grepl("\\.yaml$", keys)]
  )
  expect_equivalent(
    object,
    read_manifest(bucket, project, object@Fingerprint)
  )

  expect_is(
    stored2 <- store_manifest_yaml(
      x = object,
      base = bucket,
      project = project,
      docker = docker,
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
  expect_equivalent(
    stored2,
    available[grepl("\\.yaml$", keys)]
  )
  expect_equivalent(
    object,
    read_manifest(bucket, project, object@Fingerprint)
  )
  expect_equivalent(
    stored,
    stored2
  )
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
